{-# LANGUAGE UndecidableInstances #-}

module VName where

import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)

newtype Tag = Tag {getTag :: Int}
  deriving (Show, Eq, Ord)

initTag :: Tag
initTag = Tag 0

nextTag :: Tag -> Tag
nextTag (Tag x) = Tag $ x + 1

data VName = VName
  { varName :: Text,
    varTag :: Tag
  }
  deriving (Show, Ord)

instance Eq VName where
  VName _ tag1 == VName _ tag2 = tag1 == tag2

class (Monad m) => MonadVName m where
  getVarTag :: m Tag
  putVarTag :: Tag -> m ()

newTag :: (MonadVName m) => m Tag
newTag = do
  tag <- getVarTag
  putVarTag $ nextTag tag
  pure tag

newVName :: (MonadVName m) => Text -> m VName
newVName s = VName s <$> newTag

instance (Monad m, MonadState Tag m) => MonadVName m where
  getVarTag = get
  putVarTag = put
