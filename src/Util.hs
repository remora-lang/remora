module Util where

import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter

prettyText :: (Pretty x) => x -> Text
prettyText = T.pack . show . pretty
