module Util where

import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.String
import Prettyprinter.Render.Text

prettyText :: (Pretty x) => x -> Text
prettyText = renderStrict . layoutPretty defaultLayoutOptions . pretty

prettyString :: (Pretty x) => x -> String
prettyString = renderString . layoutPretty defaultLayoutOptions . pretty
