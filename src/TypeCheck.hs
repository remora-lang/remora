module TypeCheck (check) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import SymTable
import Syntax
import Text.Megaparsec.Pos (SourcePos)
import VName

data Env = Env
  { envMap :: Map Text VName,
    envTable :: SymTable VName (Type VName)
  }

withPos :: SourcePos -> Text -> Text
withPos pos = (<> (" at " <> T.pack (show pos)))

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

bindVName :: Text -> CheckM a -> CheckM a
bindVName v m = do
  envm <- asks envMap
  vname <-
    case envm M.!? v of
      Nothing -> newVName v
      Just vname -> pure vname
  local (\env -> env {envMap = M.insert v vname $ envMap env}) m

bindVNames :: [Text] -> CheckM a -> CheckM a
bindVNames [] m = m
bindVNames (v : vs) m =
  bindVName v $ bindVNames vs m

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

check :: Exp Unchecked Text -> Either Error (Exp Typed VName)
check e = fst $ evalRWS (runExceptT $ runCheckM $ checkExp e) initEnv initTag

-- All this does for now is convert all the `Text` variables to `VName` ones.
checkExp :: Exp Unchecked Text -> CheckM (Exp Typed VName)
checkExp (Var v _ pos) = do
  vname <- lookupVName v
  t <- lookupType vname
  pure $ Var vname (Typed t) pos
checkExp e@(Array _ [] _ pos) =
  throwError $ withPos pos $ "Empty array constructed without type:" <+> pretty e
checkExp e@(Array shape (a : as) _ pos) = do
  a' : as' <- mapM checkAtom (a : as)
  unless (all (== typeOf a') as') $
    throwError $
      withPos pos $
        "Atoms in array have different types:" <+> pretty (a' : as')
  shape' <- checkIdx shape
  -- TODO: check that the # of elements matches the shape
  Array shape (a' : as') (TArr (typeOf a') shape') pos
checkExp (EmptyArray shape t _ pos) = do
  t' <- checkType t
  shape' <- checkIdx shape
  pure $ EmptyArray shape t' (TArr t' shape') pos
checkExp e@(Frame _ [] _ pos) =
  throwError $ withPos pos $ "Empty frame constructed without type:" <+> pretty e
checkExp (Frame shape (e : es) _ pos) = do
  e' : es' <- mapM checkAtom (e : es)
  unless (all (== typeOf e') es') $
    throwError $
      withPos pos $
        "Expressions in frame have different types:" <+> pretty (e' : es')
  shape' <- checkIdx shape
  -- TODO: check that the # of elements matches the shape
  Frame shape (e' : es') (TArr (typeOf e') shape') pos
checkExp (EmptyFrame shape t _ pos) = do
  t' <- checkType t
  shape' <- checkIdx shape
  pure $ EmptyFrame shape t' (TArr t' shape') pos
checkExp app@(App (f : e : es) _ pos) = do
  (f' : e' : es') <- mapM checkExp (f : e : es)
  case typeOf f' of
    pts :-> ret -> do
      let argts = mapM typeOf (e' : es')
      unless (and $ zipWith (==) pts argts) $ -- this will have to be relaxed
        throwError $
          withPos pos $
            "Parameter and argument types don't match:" <+> pretty app
      pure $ App (f' : e' : es') (Typed ret) pos
    _ ->
      throwError $
        withPos pos $
          "Expected a function type in application:" <+> pretty e
checkExp e@(App _ _ pos) =
  throwError $
    withPos pos $
      "Applications require at least a function an an argument:" <+> pretty e
checkExp (TApp e ts pos) = undefined
checkExp (IApp e is pos) = undefined
checkExp (Unbox vs e b _ pos) = do
  e' <- checkExp e
  bindVNames vs $
    Unbox <$> mapM lookupVName vs <*> pure e' <*> checkExp b <*> pure pos
checkExp (Atom a) = Atom <$> checkAtom a

checkAtom :: Atom Unchecked Text -> CheckM (Atom Typed VName)
checkAtom (Base b pos) = pure $ Base b pos
checkAtom (Lambda ps e pos) =
  Lambda
    <$> mapM (\(v, t) -> (,) <$> lookupVName v <*> checkType t) ps
    <*> bindVNames (map fst ps) (checkExp e)
    <*> pure pos
checkAtom (TLambda ps e pos) =
  TLambda
    <$> mapM (\(v, k) -> (,) <$> lookupVName v <*> pure k) ps
    <*> bindVNames (map fst ps) (checkExp e)
    <*> pure pos
checkAtom (ILambda ps e pos) =
  ILambda
    <$> mapM (\(v, s) -> (,) <$> lookupVName v <*> pure s) ps
    <*> bindVNames (map fst ps) (checkExp e)
    <*> pure pos
checkAtom (Box is e t pos) =
  Box <$> mapM checkIdx is <*> checkExp e <*> checkType t <*> pure pos

checkType :: Type Text -> CheckM (Type VName)
checkType (TVar v) = TVar <$> lookupVName v
checkType Bool = pure Bool
checkType Int = pure Int
checkType Float = pure Float

checkIdx :: Idx Text -> CheckM (Idx VName)
checkIdx (IdxVar v pos) = IdxVar <$> lookupVName v <*> pure pos
checkIdx (Dim d pos) = pure $ Dim d pos
checkIdx (Shape is pos) = Shape <$> mapM checkIdx is <*> pure pos
checkIdx (Add is pos) = Add <$> mapM checkIdx is <*> pure pos
checkIdx (Concat is pos) = Concat <$> mapM checkIdx is <*> pure pos
