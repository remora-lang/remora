module Pass where

import Control.Monad.Error.Class
import Control.Monad.State
import Intrinsics
import Util
import VName

newtype PassM a = PassM {runPassM :: StateT Tag (Either Error) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Tag,
      MonadError Error
    )

runPass :: PassM a -> Either Error a
runPass = flip evalStateT maxIntrinsicTag . runPassM
