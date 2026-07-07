module Pass where

import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.State
import Data.Functor.Identity
import Intrinsics
import Util
import VName

newtype PassT m a = PassT {unPassT :: StateT Tag (ExceptT Error m) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Tag,
      MonadError Error
    )

deriving instance (MonadIO m) => MonadIO (PassT m)

instance MonadTrans PassT where
  lift = PassT . lift . lift

type PassM = PassT Identity

runPassT :: (Monad m) => PassT m a -> m (Either Error a)
runPassT = runExceptT . flip evalStateT maxIntrinsicTag . unPassT

runPass :: PassM a -> Either Error a
runPass = runIdentity . runPassT

runPassIO :: PassT IO a -> IO (Either Error a)
runPassIO = runPassT
