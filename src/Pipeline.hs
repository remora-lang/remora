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
    PassAST (..),
    compileWithASTs,
    compileWithASTsExp,
    interpret,
    interpretExp,
  )
where

import Control.Monad
import Data.Aeson (ToJSON, Value, toJSON)
import Data.Text (Text)
import Defunctionalize qualified
import Futhark qualified
import Interpreter qualified
import LambdaLift qualified
import Monomorphize qualified
import Pass
import Serializer ()
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
compileM = fmap snd . compileWithASTsM

compileExpM :: UncheckedExp -> PassM Text
compileExpM = fmap snd . compileWithASTsExpM

-- | The AST produced by one pass of the compilation pipeline,
-- serialized to JSON, and the name of the corresponding pass.
data PassAST = PassAST
  { passName :: String,
    passJSON :: Value
  }

mkPassAST :: (ToJSON a) => String -> a -> PassAST
mkPassAST name = PassAST name . toJSON

compileWithASTs :: UncheckedProg -> Either Error ([PassAST], Text)
compileWithASTs = runPass . compileWithASTsM

compileWithASTsExp :: UncheckedExp -> Either Error ([PassAST], Text)
compileWithASTsExp = runPass . compileWithASTsExpM

compileWithASTsM :: UncheckedProg -> PassM ([PassAST], Text)
compileWithASTsM parsed = do
  uniquified <- Uniquify.uniquify parsed
  checked <- TypeCheck.check uniquified
  lifted <- LambdaLift.lambdaLift checked
  monomorphized <- Monomorphize.monomorphize lifted
  defunctionalized <- Defunctionalize.defunctionalize monomorphized
  ir <- Futhark.compile defunctionalized
  pure
    ( [ mkPassAST "parse" parsed,
        mkPassAST "uniquify" uniquified,
        mkPassAST "typecheck" checked,
        mkPassAST "lambdalift" lifted,
        mkPassAST "monomorphize" monomorphized,
        mkPassAST "defunctionalize" defunctionalized
      ],
      ir
    )

compileWithASTsExpM :: UncheckedExp -> PassM ([PassAST], Text)
compileWithASTsExpM parsed = do
  uniquified <- Uniquify.uniquifyExp parsed
  checked <- TypeCheck.checkExp uniquified
  lifted <- LambdaLift.lambdaLiftExp checked
  monomorphized <- Monomorphize.monomorphizeExp lifted
  defunctionalized <- Defunctionalize.defunctionalizeExp monomorphized
  ir <- Futhark.compileExp defunctionalized
  pure
    ( [ mkPassAST "parse" parsed,
        mkPassAST "uniquify" uniquified,
        mkPassAST "typecheck" checked,
        mkPassAST "lambdalift" lifted,
        mkPassAST "monomorphize" monomorphized,
        mkPassAST "defunctionalize" defunctionalized
      ],
      ir
    )

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
