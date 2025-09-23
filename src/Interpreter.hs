module Interpreter where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text
import Syntax hiding (Atom, Exp, Idx, Type)
import Syntax qualified
import VName

type Exp = Syntax.Exp Typed VName

type Atom = Syntax.Atom Typed VName

type Idx = Syntax.Idx VName

type Type = Syntax.Type VName

data Env = Env
  { envMap :: Map VName Exp
  }

type Error = Text

newtype InterpM a = InterpM {runInterpM :: ExceptT Error (Reader Env) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error
    )

data Val
  = ValVar Text
  | ValArray [Int] AtVal

data AtVal
  = ValBase Base
  | ValLambda [(VName, Type)] Exp
  | ValTLambda [(VName, Kind)] Exp
  | ValILambda [(VName, Sort)] Exp
  | ValBox [Idx] Exp Type

int :: Exp -> InterpM AtVal
int = undefined

intAtom :: Atom -> InterpM AtVal
intAtom (Base b _ _) = pure $ ValBase b
