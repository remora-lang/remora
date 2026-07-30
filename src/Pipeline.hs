module Pipeline
  ( typeCheck,
    typeCheckExp,
    uniquify,
    uniquifyExp,
    monomorphize,
    monomorphizeExp,
    lambdaLift,
    lambdaLiftExp,
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
import LambdaLift qualified
import Monomorphize qualified
import Pass
import Syntax
import TypeCheck qualified
import Uniquify qualified
import Util

uniquify :: UncheckedProg -> Either Error UniqueProg
uniquify = runPass . Uniquify.uniquify

uniquifyExp :: UncheckedExp -> Either Error UniqueExp
uniquifyExp = runPass . Uniquify.uniquifyExp

typeCheck :: UncheckedProg -> Either Error Prog
typeCheck = runPass . typeCheckM

typeCheckExp :: UncheckedExp -> Either Error Exp
typeCheckExp = runPass . typeCheckExpM

monomorphize :: UncheckedProg -> Either Error Prog
monomorphize = runPass . monomorphizeM

monomorphizeExp :: UncheckedExp -> Either Error Exp
monomorphizeExp = runPass . monomorphizeExpM

lambdaLift :: UncheckedProg -> Either Error Prog
lambdaLift = runPass . lambdaLiftM

lambdaLiftExp :: UncheckedExp -> Either Error Exp
lambdaLiftExp = runPass . lambdaLiftExpM

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

typeCheckExpM :: UncheckedExp -> PassM Exp
typeCheckExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp

lambdaLiftM :: UncheckedProg -> PassM Prog
lambdaLiftM =
  Uniquify.uniquify
    >=> TypeCheck.check
    >=> LambdaLift.lambdaLift

lambdaLiftExpM :: UncheckedExp -> PassM Exp
lambdaLiftExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> LambdaLift.lambdaLiftExp

monomorphizeM :: UncheckedProg -> PassM Prog
monomorphizeM =
  Uniquify.uniquify
    >=> TypeCheck.check
    >=> LambdaLift.lambdaLift
    >=> Monomorphize.monomorphize

monomorphizeExpM :: UncheckedExp -> PassM Exp
monomorphizeExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> LambdaLift.lambdaLiftExp
    >=> Monomorphize.monomorphizeExp

compileM :: UncheckedProg -> PassM Text
compileM =
  Uniquify.uniquify
    >=> TypeCheck.check
    >=> LambdaLift.lambdaLift
    >=> Monomorphize.monomorphize
    >=> Futhark.compile

compileExpM :: UncheckedExp -> PassM Text
compileExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> LambdaLift.lambdaLiftExp
    >=> Monomorphize.monomorphizeExp
    >=> Futhark.compileExp

interpretM :: [Interpreter.Val] -> UncheckedProg -> PassM Interpreter.Val
interpretM args =
  Uniquify.uniquify
    >=> TypeCheck.check
    >=> Interpreter.interpret args

interpretExpM :: UncheckedExp -> PassM Interpreter.Val
interpretExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> Interpreter.interpretExp
