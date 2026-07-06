module Pipeline
  ( typeCheck,
    monomorphize,
    compile,
    compileExp,
    interpret,
    interpretExp,
  )
where

import Control.Monad
import Data.Text (Text)
import Futhark qualified
import Interpreter qualified
import Monomorphize qualified
import Pass
import Syntax
import TypeCheck qualified
import Uniquify qualified
import Util

typeCheck :: UncheckedProg -> Either Error Prog
typeCheck = runPass . typeCheckM

monomorphize :: UncheckedProg -> Either Error Prog
monomorphize = runPass . monomorphizeM

compile :: UncheckedProg -> Either Error Text
compile = runPass . compileM

compileExp :: UncheckedExp -> Either Error Text
compileExp = runPass . compileExpM

interpret :: [Interpreter.Val] -> UncheckedProg -> Either Error Interpreter.Val
interpret args = runPass . interpretM args

interpretExp :: UncheckedExp -> Either Error Interpreter.Val
interpretExp = runPass . interpretExpM

typeCheckM :: UncheckedProg -> PassM Prog
typeCheckM =
  Uniquify.uniquify
    >=> TypeCheck.check

monomorphizeM :: UncheckedProg -> PassM Prog
monomorphizeM =
  typeCheckM
    >=> Monomorphize.monomorphize

compileM :: UncheckedProg -> PassM Text
compileM =
  typeCheckM
    >=> Monomorphize.monomorphize
    >=> Futhark.compile

compileExpM :: UncheckedExp -> PassM Text
compileExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> Monomorphize.monomorphizeExp
    >=> Futhark.compileExp

interpretM :: [Interpreter.Val] -> UncheckedProg -> PassM Interpreter.Val
interpretM args =
  typeCheckM
    >=> Interpreter.interpret args

interpretExpM :: UncheckedExp -> PassM Interpreter.Val
interpretExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> Interpreter.interpretExp
