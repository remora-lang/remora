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
import SymTable
import Syntax
import Text.Megaparsec.Pos (SourcePos)
import Util
import VName

data Env = Env
  { envMap :: Map Text VName,
    envTable :: SymTable VName (Type VName)
  }

withPos :: (Pretty x) => SourcePos -> x -> Text
withPos pos x = prettyText x <> (" at " <> prettyText (show pos))

initEnv :: Env
initEnv = Env mempty mempty

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
    Nothing -> throwError $ "unknown type for var: " <> T.pack (show v)
    Just t -> pure t

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
  askSymTable = asks envTable
  withSymTable f =
    local (\env -> env {envTable = f $ envTable env})

check ::
  (Monad m) =>
  Exp Unchecked Text ->
  Either Error ([(VName, Type VName, Val m)], Exp Typed VName)
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
          unless (and $ zipWith (==) pts argts) $ -- this will have to be relaxed
            throwError $
              withPos pos $
                "Parameter and argument types don't match: " <> prettyText app
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
checkExp (TApp e ts _ pos) = undefined
checkExp (IApp e is _ pos) = undefined
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
checkType (as :-> b) = (:->) <$> mapM checkType as <*> checkType b

checkIdx :: Idx Text -> CheckM (Idx VName)
checkIdx (IdxVar v) = IdxVar <$> lookupVName v
checkIdx (Dim d) = pure $ Dim d
checkIdx (Shape is) = Shape <$> mapM checkIdx is
checkIdx (Add is) = Add <$> mapM checkIdx is
checkIdx (Concat is) = Concat <$> mapM checkIdx is

withPrelude :: (Monad m) => CheckM a -> CheckM ([(VName, Type VName, Val m)], a)
withPrelude m = checkPrelude prelude mempty
  where
    checkPrelude [] prelude' =
      (reverse prelude',) <$> m
    checkPrelude ((f, t, v) : ps) prelude' = do
      t' <- checkType t
      bindVName f t' $ do
        f' <- lookupVName f
        checkPrelude ps ((f', t', v) : prelude')
