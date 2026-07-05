module Pipeline
  ( typeCheck,
    compile,
    interpret,
  )
where

import Control.Monad
import Data.Text (Text)
import Futhark qualified
import Interpreter qualified
import Pass
import Syntax
import TypeCheck qualified
import Uniquify qualified
import Util

typeCheck :: UncheckedExp -> Either Error Exp
typeCheck = runPass . typeCheckM

compile :: UncheckedExp -> Either Error Text
compile = runPass . compileM

interpret :: UncheckedExp -> Either Error Interpreter.Val
interpret = runPass . interpretM

typeCheckM :: UncheckedExp -> PassM Exp
typeCheckM =
  Uniquify.uniquify
    >=> TypeCheck.check

compileM :: UncheckedExp -> PassM Text
compileM =
  typeCheckM
    >=> Futhark.compile

interpretM :: UncheckedExp -> PassM Interpreter.Val
interpretM =
  typeCheckM
    >=> Interpreter.interpret
