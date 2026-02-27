module Pipeline
  ( typeCheck,
    compile,
    interpret,
  )
where

import Control.Monad
import Data.Text (Text)
import Data.Text qualified as T
import Futhark qualified
import Interpreter qualified
import Syntax
import TypeCheck qualified
import Uniquify qualified
import Util
import VName

type Error = T.Text

typeCheck :: Exp NoInfo Text -> Either Error (Exp Info VName)
typeCheck =
  Uniquify.uniquify
    .> TypeCheck.check

compile :: Exp NoInfo Text -> Either Error Text
compile =
  typeCheck
    >=> Futhark.compile

interpret :: Exp NoInfo Text -> Either Error Interpreter.Val
interpret =
  typeCheck
    >=> Interpreter.interpret
