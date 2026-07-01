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

type Error = T.Text

typeCheck :: UncheckedExp -> Either Error Exp
typeCheck =
  Uniquify.uniquify
    .> TypeCheck.check

compile :: UncheckedExp -> Either Error Text
compile =
  typeCheck
    >=> Futhark.compile

interpret :: UncheckedExp -> Either Error Interpreter.Val
interpret =
  typeCheck
    >=> Interpreter.interpret
