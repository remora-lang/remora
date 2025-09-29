module TypeCheck (check) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Interpreter.Value
import Prettyprinter
import RemoraPrelude
import Substitute
import SymTable
import Syntax
import Text.Megaparsec.Pos (SourcePos)
import Util
import VName

data Env = Env
  { envMap :: Map Text VName,
    envType :: SymTable VName (Type VName),
    envKind :: SymTable VName Kind
  }

withPos :: (Pretty x) => SourcePos -> x -> Text
withPos pos x = prettyText x <> (" at " <> prettyText (show pos))

initEnv :: Env
initEnv = Env mempty mempty mempty

lookupVName :: Text -> CheckM VName
lookupVName v = do
  mvname <- asks $ (M.!? v) . envMap
  case mvname of
    Nothing -> throwError $ "unknown var: " <> v
    Just vname -> pure vname

lookupType :: VName -> CheckM (Type VName)
lookupType v = do
  mt <- askSym v
  case mt of
    Nothing -> throwError $ "unknown type for var: " <> prettyText v
    Just t -> pure t

kindOf :: Type VName -> CheckM Kind
kindOf (TVar t) = do
  mk <- askSym t
  case mk of
    Nothing -> throwError $ "unknown kind for type: " <> prettyText t
    Just k -> pure k
kindOf TArr {} = pure KindArray
kindOf _ = pure KindAtom

sortOf :: Shape VName -> Sort
-- This is shit (and wrongish), fix
sortOf ShapeDim {} = SortDim
sortOf _ = SortShape

bindVName :: Text -> Type VName -> CheckM a -> CheckM a
bindVName v t m = do
  envm <- asks envMap
  vname <-
    case envm M.!? v of
      Nothing -> newVName v
      Just vname -> pure vname
  local (\env -> env {envMap = M.insert v vname $ envMap env}) $
    insertSym vname t m

bindVNames :: [(Text, Type VName)] -> CheckM a -> CheckM a
bindVNames [] m = m
bindVNames ((v, t) : vs) m =
  bindVName v t $ bindVNames vs m

bindType :: Text -> Kind -> CheckM a -> CheckM a
bindType v k m = do
  vname <- newVName v
  local (\env -> env {envMap = M.insert v vname $ envMap env}) $
    insertSym vname k m

bindTypes :: [(Text, Kind)] -> CheckM a -> CheckM a
bindTypes [] m = m
bindTypes ((v, k) : vs) m =
  bindType v k $ bindTypes vs m

bindSort :: Text -> Sort -> CheckM a -> CheckM a
bindSort v s m = do
  vname <- newVName v
  local (\env -> env {envMap = M.insert v vname $ envMap env}) m

bindSorts :: [(Text, Sort)] -> CheckM a -> CheckM a
bindSorts [] m = m
bindSorts ((v, s) : vs) m =
  bindSort v s $ bindSorts vs m

type Error = Text

newtype CheckM a = CheckM {runCheckM :: ExceptT Error (RWS Env () Tag) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState Tag,
      MonadError Error
    )

instance MonadSymTable CheckM VName (Type VName) where
  askSymTable = asks envType
  withSymTable f =
    local (\env -> env {envType = f $ envType env})

instance MonadSymTable CheckM VName Kind where
  askSymTable = asks envKind
  withSymTable f =
    local (\env -> env {envKind = f $ envKind env})

check ::
  (Monad m) =>
  Exp Unchecked Text ->
  Either Error (Prelude VName m, Exp Typed VName)
check e = fst $ evalRWS (runExceptT $ runCheckM $ withPrelude $ checkExp e) initEnv initTag

-- All this does for now is convert all the `Text` variables to `VName` ones.
checkExp :: Exp Unchecked Text -> CheckM (Exp Typed VName)
checkExp (Var v _ pos) = do
  vname <- lookupVName v
  t <- lookupType vname
  pure $ Var vname (Typed t) pos
checkExp e@(Array _ [] _ pos) =
  throwError $ withPos pos $ "Empty array constructed without type: " <> prettyText e
checkExp e@(Array shape as _ pos) = do
  as' <- mapM checkAtom as
  case as' of
    [] -> error "impossible"
    (a' : as'') -> do
      unless (all (\a'' -> typeOf a'' == typeOf a') as'') $
        throwError $
          withPos pos $
            "Atoms in array have different types: " <> prettyText as'
      -- TODO: check that the # of elements matches the shape
      pure $ Array shape as' (Typed $ TArr (typeOf a') (idxFromDims shape)) pos
checkExp (EmptyArray shape t _ pos) = do
  t' <- checkType t
  pure $ EmptyArray shape t' (Typed $ TArr t' (idxFromDims shape)) pos
checkExp e@(Frame _ [] _ pos) =
  throwError $ withPos pos $ "Empty frame constructed without type: " <> prettyText e
checkExp (Frame shape es _ pos) = do
  es' <- mapM checkExp es
  case es' of
    [] -> error "impossible"
    (e' : es'') -> do
      unless (all (\e'' -> typeOf e'' == typeOf e') es'') $
        throwError $
          withPos pos $
            "Expressions in frame have different types: " <> prettyText es'
      -- TODO: check that the # of elements matches the shape
      pure $ Frame shape es' (Typed $ TArr (typeOf e') (idxFromDims shape)) pos
checkExp (EmptyFrame shape t _ pos) = do
  t' <- checkType t
  pure $ EmptyFrame shape t' (Typed $ TArr t' (idxFromDims shape)) pos
checkExp app@(App fes@(f : e : _) _ pos) = do
  fes' <- mapM checkExp fes
  case fes' of
    (f' : es') ->
      case typeOf f' of
        pts :-> ret -> do
          let argts = map typeOf es'
          unless (pts == argts) $ -- this will have to be relaxed
            throwError $
              withPos pos $
                "Parameter and argument types don't match:\n"
                  <> prettyText app
                  <> "\n"
                  <> prettyText (pts, argts)
          pure $ App (f' : es') (Typed ret) pos
        _ ->
          throwError $
            withPos pos $
              "Expected a function type in application: " <> prettyText e
    _ -> error "impossible"
checkExp e@(App _ _ pos) =
  throwError $
    withPos pos $
      "Applications require at least a function and an argument: " <> prettyText e
checkExp tapp@(TApp e ts _ pos) = do
  e' <- checkExp e
  ts' <- mapM checkType ts
  case typeOf e' of
    Forall pts r -> do
      ks <- mapM kindOf ts'
      unless (map snd pts == ks) $
        throwError $
          withPos pos $
            "Parameter and argument kinds don't match:\n"
              <> prettyText tapp
              <> "\n"
              <> prettyText (pts, ts')
      let r' = substitute' (zip (map fst pts) ts') r
      pure $ TApp e' ts' (Typed r') pos
    t ->
      throwError $
        withPos pos $
          "Expected a forall type in type application: "
            <> prettyText tapp
            <> "\n"
            <> prettyText t
checkExp iapp@(IApp e is _ pos) = do
  e' <- checkExp e
  is' <- mapM checkShape is
  case typeOf e' of
    DProd pts r -> do
      let sorts = map sortOf is'
      unless (map snd pts == sorts) $
        throwError $
          withPos pos $
            "Parameter and argument sorts don't match:\n"
              <> prettyText iapp
              <> "\n"
              <> prettyText (map snd pts, sorts)
      let r' =
            substitute' (zip (map fst pts) (map sepDim is')) r
      pure $ IApp e' is' (Typed r') pos
    t ->
      throwError $
        withPos pos $
          "Expected a dprod type in shape application: "
            <> prettyText iapp
            <> "\n"
            <> prettyText t
checkExp (Unbox vs e b _ pos) = undefined
checkExp (Atom a) = Atom <$> checkAtom a

checkAtom :: Atom Unchecked Text -> CheckM (Atom Typed VName)
checkAtom (Base b _ pos) =
  pure $ Base b (Typed $ typeOf b) pos
checkAtom (Lambda ps e _ pos) = do
  let (xs, pts) = unzip ps
  pts' <- mapM checkType pts
  bindVNames (zip xs pts') $ do
    xs' <- mapM lookupVName xs
    e' <- checkExp e
    pure $ Lambda (zip xs' pts') e' (Typed $ pts' :-> typeOf e') pos
checkAtom (TLambda ps e _ pos) = undefined
checkAtom (ILambda ps e _ pos) = undefined
checkAtom (Box is e t pos) = undefined

checkType :: Type Text -> CheckM (Type VName)
checkType (TVar v) = TVar <$> lookupVName v
checkType Bool = pure Bool
checkType Int = pure Int
checkType Float = pure Float
checkType (TArr t shape) = TArr <$> checkType t <*> checkShape shape
checkType (as :-> b) = (:->) <$> mapM checkType as <*> checkType b
checkType (Forall pts t) =
  bindTypes pts $ do
    pts' <- forM pts $ \(v, k) -> (,k) <$> lookupVName v
    t' <- checkType t
    pure $ Forall pts' t'
checkType (DProd pts t) =
  bindSorts pts $ do
    pts' <- forM pts $ \(v, s) -> (,s) <$> lookupVName v
    t' <- checkType t
    pure $ DProd pts' t'

checkDim :: Dim Text -> CheckM (Dim VName)
checkDim (DimVar v) = DimVar <$> lookupVName v
checkDim (Dim d) = pure $ Dim d
checkDim (Add ds) = Add <$> mapM checkDim ds

checkShape :: Shape Text -> CheckM (Shape VName)
checkShape (ShapeVar v) = ShapeVar <$> lookupVName v
checkShape (ShapeDim d) = ShapeDim <$> checkDim d
checkShape (Concat ss) = Concat <$> mapM checkShape ss

withPrelude :: (Monad m) => CheckM a -> CheckM (Prelude VName m, a)
withPrelude m = checkPrelude prelude mempty
  where
    checkPrelude [] prelude' =
      (reverse prelude',) <$> m
    checkPrelude (p : ps) prelude' = do
      case p of
        PreludeVal f t v -> do
          t' <- checkType t
          bindVName f t' $ do
            f' <- lookupVName f
            checkPrelude ps (PreludeVal f' t' v : prelude')
        PreludeType t k ->
          bindType t k $ do
            t' <- lookupVName t
            checkPrelude ps (PreludeType t' k : prelude')
