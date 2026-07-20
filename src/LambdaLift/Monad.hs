module LambdaLift.Monad where

import Binds
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Map (Map)
import Data.Map qualified as M
import Prop (arrayTypeOf)
import Syntax hiding (ArrayType)
import Syntax qualified
import Util (Error)
import VName

type ArrayType = Syntax.ArrayType VName

newtype Env = Env {envVtable :: Map VName ArrayType}

initEnv :: Env
initEnv = Env mempty

data St = St
  { stateLiftBinds :: [Bind],
    stateTag :: Tag
  }

initSt :: Tag -> St
initSt = St mempty

instance HasBinds St where
  getBinds = stateLiftBinds
  putBinds bs st = st {stateLiftBinds = bs}

newtype LiftM a = LiftM {runLiftM :: ReaderT Env (ExceptT Error (State St)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState St,
      MonadError Error
    )

runLift :: Tag -> LiftM a -> (Either Error a, Tag)
runLift tag =
  second stateTag
    . flip runState (initSt tag)
    . runExceptT
    . flip runReaderT initEnv
    . runLiftM

instance MonadVName LiftM where
  getVarTag = gets stateTag
  putVarTag tag = modify $ \st -> st {stateTag = tag}

lookupType :: VName -> LiftM (Maybe ArrayType)
lookupType v = asks (M.lookup v . envVtable)

bindType :: VName -> ArrayType -> LiftM a -> LiftM a
bindType v t = local $
  \env -> env {envVtable = M.insert v t $ envVtable env}

bindTypes :: [(VName, ArrayType)] -> LiftM a -> LiftM a
bindTypes vts = local $
  \env -> env {envVtable = M.fromList vts <> envVtable env}

withBind :: Bind -> LiftM a -> LiftM a
withBind (BindVal v _ e _) = bindType v $ arrayTypeOf e
withBind (BindFun f _ _ _ (Info ft) _) = bindType f $ mkScalarArrayType ft
withBind _ = id

withBinds :: [Bind] -> LiftM a -> LiftM a
withBinds bs m = foldr withBind m bs
