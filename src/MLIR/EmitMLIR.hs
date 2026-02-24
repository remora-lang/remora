{- Walk Futhark IR and emit something to get us to MLIR

   IR syntax: github/futhark:src/Futhark/IR/Syntax.hs
          see, e.g. "data Exp rep ..."
   SOACS:     github/futhark:src/Futhark/IR/SOACS/SOAC.hs
   IR Parser: github/futhark:src/Futhark/IR/Parse.hs
-}

module MLIR.EmitMLIR where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Except
import Futhark.IR.SOACS  -- re-exports Syntax, Prop, Prop, Pretty, Traversals
import Futhark.MonadFreshNames
import Data.Map qualified as M
import Data.Map (Map)
import Data.Text qualified as T

type Error = String

data Env
  = Env { envVarMap :: M.Map VName Val }

initEnv :: Env
initEnv = Env M.empty

data State = State
  { stateString :: String,
    stateVNameSource :: VNameSource
  }

initState :: State
initState = State { stateString = "",
                    stateVNameSource = blankNameSource
                  }
data Val = ValPrim PrimValue
  deriving (Show)

newtype MLIRM a = MLIRM {runMLIRM :: StateT State (ReaderT Env (Except Error)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState State
    )

instance MonadFreshNames MLIRM where
  getNameSource = gets stateVNameSource
  putNameSource new = modify (\ s -> s{stateVNameSource = new})


lookupVar :: VName -> MLIRM Val
lookupVar vname = do
  mval <- asks $ (M.!? vname) . envVarMap
  case mval of
    Nothing -> error $ "unbound variable: " <> show vname
    Just val -> pure val

emit :: String -> MLIRM ()
emit str = modify $ \s -> s {stateString = stateString s ++ str}

intExp :: Exp (SOACS) -> MLIRM Val
intExp (BasicOp op) = intBasicOp op
intExp (Op soac) = intSOAC soac

intBasicOp :: BasicOp -> MLIRM Val
intBasicOp (SubExp se) = intSubExp se

-- A subexpression is either a scalar constant or a variable
intSubExp :: SubExp -> MLIRM Val
intSubExp (Constant pv) = pure $ ValPrim pv
intSubExp (Var x) = do
  xVal <- lookupVar x
  emit $ "var " ++ (show x) ++ " bound to " ++ (show xVal)
  pure xVal

intSOAC :: SOAC SOACS -> MLIRM Val
intSOAC (Screma size xs (ScremaForm lambda [] [])) = undefined

varRefEx2 :: MLIRM ()
varRefEx2 = do
  vname <- newVName (nameFromString "myvar")
  local (\ env -> env { envVarMap = M.insert vname (ValPrim UnitValue) (envVarMap env) }) $
    void $ intSubExp (Var vname)


emitMLIR :: MLIRM () -> String
emitMLIR m = case (runExcept (runReaderT (execStateT (runMLIRM m) initState) initEnv)) of
  Left err -> error err
  Right s -> stateString s
  
main :: IO ()
main =
  putStrLn $ emitMLIR varRefEx2
  
