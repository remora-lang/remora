module TypeCheck (check) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.SBV qualified as SBV
import Data.Text (Text)
import Data.Text qualified as T
import Interpreter.Value
import Prettyprinter
import RemoraPrelude
import Substitute
import SymTable
import Symbolic
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

lookupVName :: (MonadCheck m) => Text -> m VName
lookupVName v = do
  mvname <- asks $ (M.!? v) . envMap
  case mvname of
    Nothing -> throwError $ "unknown var: " <> v
    Just vname -> pure vname

lookupType :: (MonadCheck m) => VName -> m (Type VName)
lookupType v = do
  mt <- askSym v
  case mt of
    Nothing -> throwError $ "unknown type for var: " <> prettyText v
    Just t -> pure t

kindOf :: (MonadCheck m) => Type VName -> m Kind
kindOf (TVar t) = do
  mk <- askSym t
  case mk of
    Nothing -> throwError $ "unknown kind for type: " <> prettyText t
    Just k -> pure k
kindOf TArr {} = pure KindArray
kindOf _ = pure KindAtom

bindVName :: (MonadCheck m) => Text -> Type VName -> m a -> m a
bindVName v t m = do
  envm <- asks envMap
  vname <-
    case envm M.!? v of
      Nothing -> newVName v
      Just vname -> pure vname
  local (\env -> env {envMap = M.insert v vname $ envMap env}) $
    insertSym vname t m

bindVNames :: (MonadCheck m) => [(Text, Type VName)] -> m a -> m a
bindVNames [] m = m
bindVNames ((v, t) : vs) m =
  bindVName v t $ bindVNames vs m

bindType :: (MonadCheck m) => Text -> Kind -> m a -> m a
bindType v k m = do
  vname <- newVName v
  local (\env -> env {envMap = M.insert v vname $ envMap env}) $
    insertSym vname k m

bindTypes :: (MonadCheck m) => [(Text, Kind)] -> m a -> m a
bindTypes [] m = m
bindTypes ((v, k) : vs) m =
  bindType v k $ bindTypes vs m

bindSort :: (MonadCheck m) => Text -> Sort -> m a -> m a
bindSort v s m = do
  vname <- newVName v
  local (\env -> env {envMap = M.insert v vname $ envMap env}) m

bindSorts :: (MonadCheck m) => [(Text, Sort)] -> m a -> m a
bindSorts [] m = m
bindSorts ((v, s) : vs) m =
  bindSort v s $ bindSorts vs m

type Error = Text

type MonadCheck m =
  ( Monad m,
    MonadReader Env m,
    MonadState Tag m,
    MonadError Error m,
    MonadSymTable m VName (Type VName),
    MonadSymTable m VName Kind
  )

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

checkExp :: (MonadCheck m) => Exp Unchecked Text -> m (Exp Typed VName)
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
      case normType $ typeOf f' of
        -- TArr (pts :-> ret) frame_f -> do
        --   (arg_shapes, substs, frames) <-
        --     unzip3
        --       <$> ( forM (zip pts es') $ \(arg_t, e') -> do
        --               frame_a <- ShapeVar <$> newVName "a"
        --               let (s, subst) =
        --                     satShapes
        --                       (SBV..==)
        --                       (frame_a <> shapeOf arg_t)
        --                       (shapeOf e')
        --               pure (s, subst, frame_a)
        --           )
        --   let frames' = zipWith substitute substs frames
        --       principal = maximumShape $ map (frame_f <>) frames'
        --       ret' = TArr ret principal
        --   pure $ App (f' : es') (Typed (ret', principal)) pos
        TArr (pts :-> ret) frame_f -> do
          (arg_shapes, frames) <-
            unzip
              <$> ( forM (zip pts es') $ \(arg_t, e') -> do
                      case normShape (shapeOf e') \\ normShape (shapeOf arg_t) of
                        Nothing ->
                          throwError $
                            "Incompatible: "
                              <> prettyText (shapeOf e')
                              <> "\n"
                              <> prettyText (shapeOf arg_t)
                        Just frame_a ->
                          pure (shapeOf e', frame_a)
                  )
          let principal = maximumShape $ map (frame_f <>) frames
              ret' = TArr ret principal
          pure $ App (f' : es') (Typed (ret', principal)) pos
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
    TArr (Forall pts r) frame_f -> do
      ks <- mapM kindOf ts'
      unless (map snd pts == ks) $
        throwError $
          withPos pos $
            "Parameter and argument kinds don't match:\n"
              <> prettyText tapp
              <> "\n"
              <> prettyText (pts, ts')
      let r' = substitute' (zip (map fst pts) ts') r
      pure $ TApp e' ts' (Typed (TArr r' frame_f)) pos
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
      unless (all (uncurry compatSort) (zip (map snd pts) is')) $
        throwError $
          withPos pos $
            "Parameter and argument sorts don't match:\n"
              <> prettyText iapp
              <> "\n"
              <> prettyText (map snd pts, is')
      let r' =
            substitute' (zip (map fst pts) is') r
      pure $ IApp e' is' (Typed r') pos
    TArr (DProd pts r) frame_f -> do
      unless (all (uncurry compatSort) (zip (map snd pts) is')) $
        throwError $
          withPos pos $
            "Parameter and argument sorts don't match:\n"
              <> prettyText iapp
              <> "\n"
              <> prettyText (map snd pts, is')
      let r' =
            substitute' (zip (map fst pts) is') r
      pure $ IApp e' is' (Typed (TArr r' frame_f)) pos
    t ->
      throwError $
        withPos pos $
          "Expected a dprod type in shape application: "
            <> prettyText iapp
            <> "\n"
            <> prettyText t
checkExp (Unbox vs e b _ pos) = undefined

checkAtom :: (MonadCheck m) => Atom Unchecked Text -> m (Atom Typed VName)
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

checkType :: (MonadCheck m) => Type Text -> m (Type VName)
checkType = fmap normType . checkType'
  where
    checkType' (TVar v) = TVar <$> lookupVName v
    checkType' Bool = pure Bool
    checkType' Int = pure Int
    checkType' Float = pure Float
    checkType' (TArr t shape) = TArr <$> checkType' t <*> checkShape shape
    checkType' (as :-> b) = (:->) <$> mapM checkType' as <*> checkType' b
    checkType' (Forall pts t) =
      bindTypes pts $ do
        pts' <- forM pts $ \(v, k) -> (,k) <$> lookupVName v
        t' <- checkType' t
        pure $ Forall pts' t'
    checkType' (DProd pts t) =
      bindSorts pts $ do
        pts' <- forM pts $ \(v, s) -> (,s) <$> lookupVName v
        t' <- checkType' t
        pure $ DProd pts' t'

checkDim :: (MonadCheck m) => Dim Text -> m (Dim VName)
checkDim = fmap normDim . checkDim'
  where
    checkDim' (DimVar v) = DimVar <$> lookupVName v
    checkDim' (Dim d) = pure $ Dim d
    checkDim' (Add ds) = Add <$> mapM checkDim' ds

checkShape :: (MonadCheck m) => Shape Text -> m (Shape VName)
checkShape = fmap normShape . checkShape'
  where
    checkShape' (ShapeVar v) = ShapeVar <$> lookupVName v
    checkShape' (ShapeDim d) = ShapeDim <$> checkDim d
    checkShape' (Concat ss) = Concat <$> mapM checkShape' ss

withPrelude :: (MonadCheck m, Monad n) => m a -> m (Prelude VName n, a)
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
