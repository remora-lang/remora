module TypeCheck (check) where

import Control.Monad.Error.Class
import Control.Monad.RWS
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Syntax
import VName

data Env = Env
  { envMap :: Map Text VName
  }

initEnv :: Env
initEnv = Env mempty

lookupVName :: Text -> CheckM VName
lookupVName v = do
  mvname <- asks $ (M.!? v) . envMap
  case mvname of
    Nothing -> throwError $ "unknown var: " <> v
    Just vname -> pure vname

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

check :: Exp Text -> Either Error (Exp VName)
check e = fst $ evalRWS (runExceptT $ runCheckM $ checkExp e) initEnv initTag

-- All this does for now is convert all the `Text` variables to `VName` ones.
checkExp :: Exp Text -> CheckM (Exp VName)
checkExp (Var v pos) =
  Var <$> lookupVName v <*> pure pos
checkExp (Array shape as pos) =
  Array shape <$> mapM checkAtom as <*> pure pos
checkExp (EmptyArray shape t pos) =
  EmptyArray shape <$> checkType t <*> pure pos
checkExp (Frame shape es pos) =
  Frame shape <$> mapM checkExp es <*> pure pos
checkExp (EmptyFrame shape t pos) =
  EmptyFrame shape <$> checkType t <*> pure pos
checkExp (App es pos) =
  App <$> mapM checkExp es <*> pure pos
checkExp (TApp e ts pos) =
  TApp <$> checkExp e <*> mapM checkType ts <*> pure pos
checkExp (IApp e is pos) =
  IApp <$> checkExp e <*> mapM checkIdx is <*> pure pos
checkExp (Unbox vs e b pos) = do
  e' <- checkExp e
  bindVNames vs $
    Unbox <$> mapM lookupVName vs <*> pure e' <*> checkExp b <*> pure pos
checkExp (Atom a) = Atom <$> checkAtom a

checkAtom :: Atom Text -> CheckM (Atom VName)
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
