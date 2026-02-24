import Futhark.IR.SOACS -- re-exports Syntax, Prop, Prop, Pretty, Traversals
import MLIR.EmitMLIR qualified as E

varRefEx1 :: Exp (SOAC SOACS)
varRefEx1 = BasicOp (SubExp (Var "x"))

varRefEx2 :: MLIRM ()
varRefEx2 = do
  vname <- newVName (stringToName "myvar")
  local (\ env -> M.insert vname (ValPrim UnitValue)) $
    intSubExp (Var vname)

emitMLIR :: MLIRM () -> String
emitMLIR m = case (runExcept (runReaderT (execStateT (runMLIRM m) initState) initEnv)) of
  Left err -> error err
  Right s -> stateString s
  
main :: IO ()
main =
  putStrLn $ emitMLIR varRefEx2
  
