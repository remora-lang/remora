module Pipeline
  ( uniquify,
    uniquifyExp,
    typeCheck,
    typeCheckExp,
    lambdaLift,
    lambdaLiftExp,
    monomorphize,
    monomorphizeExp,
    defunctionalize,
    defunctionalizeExp,
    compile,
    compileExp,
    interpret,
    interpretExp,
  )
where

import Control.Monad
import Data.Text (Text)
import Defunctionalize qualified
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

typeCheckM :: UncheckedProg -> PassM Prog
typeCheckM =
  Uniquify.uniquify
    >=> TypeCheck.check

typeCheckExpM :: UncheckedExp -> PassM Exp
typeCheckExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp

lambdaLift :: UncheckedProg -> Either Error Prog
lambdaLift = runPass . lambdaLiftM

lambdaLiftExp :: UncheckedExp -> Either Error Exp
lambdaLiftExp = runPass . lambdaLiftExpM

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

monomorphize :: UncheckedProg -> Either Error Prog
monomorphize = runPass . monomorphizeM

monomorphizeExp :: UncheckedExp -> Either Error Exp
monomorphizeExp = runPass . monomorphizeExpM

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

defunctionalize :: UncheckedProg -> Either Error Prog
defunctionalize = runPass . defunctionalizeM

defunctionalizeExp :: UncheckedExp -> Either Error Exp
defunctionalizeExp = runPass . defunctionalizeExpM

defunctionalizeM :: UncheckedProg -> PassM Prog
defunctionalizeM =
  Uniquify.uniquify
    >=> TypeCheck.check
    >=> LambdaLift.lambdaLift
    >=> Monomorphize.monomorphize
    >=> Defunctionalize.defunctionalize

defunctionalizeExpM :: UncheckedExp -> PassM Exp
defunctionalizeExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> LambdaLift.lambdaLiftExp
    >=> Monomorphize.monomorphizeExp
    >=> Defunctionalize.defunctionalizeExp

compile :: UncheckedProg -> Either Error Text
compile = runPass . compileM

compileExp :: UncheckedExp -> Either Error Text
compileExp = runPass . compileExpM

compileM :: UncheckedProg -> PassM Text
compileM =
  Uniquify.uniquify
    >=> TypeCheck.check
    >=> LambdaLift.lambdaLift
    >=> Monomorphize.monomorphize
    >=> Defunctionalize.defunctionalize
    >=> Futhark.compile

compileExpM :: UncheckedExp -> PassM Text
compileExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> LambdaLift.lambdaLiftExp
    >=> Monomorphize.monomorphizeExp
    >=> Defunctionalize.defunctionalizeExp
    >=> Futhark.compileExp

interpret :: Text -> [Interpreter.Val] -> UncheckedProg -> Either Error Interpreter.Val
interpret entry args = runPass . interpretM entry args

interpretExp :: UncheckedExp -> Either Error Interpreter.Val
interpretExp = runPass . interpretExpM

interpretM :: Text -> [Interpreter.Val] -> UncheckedProg -> PassM Interpreter.Val
interpretM entry args =
  Uniquify.uniquify
    >=> TypeCheck.check
    >=> Interpreter.interpret entry args

interpretExpM :: UncheckedExp -> PassM Interpreter.Val
interpretExpM =
  Uniquify.uniquifyExp
    >=> TypeCheck.checkExp
    >=> Interpreter.interpretExp
