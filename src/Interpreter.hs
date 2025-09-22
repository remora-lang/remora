module Interpreter where

import Control.Monad.Reader
import Data.Map (Map)
import Data.Map qualified as M
import Data.Proxy
import Data.Text
import Syntax

data Env f v = Env
  { envMap :: Map Text (Exp f v)
  }

type InterpM f v = Reader (Env f v)

data Val f v
  = ValVar Text
  | ValArray [Int] (AtVal f v)

data AtVal f v
  = ValBase Base
  | ValLambda [(v, Maybe (Type v))] (Exp f v)
  | ValTLambda [(v, Maybe Kind)] (Exp f v)
  | ValILambda [(v, Maybe Sort)] (Exp f v)
  | ValBox [Idx v] (Exp f v) (Maybe (Type v))

int :: Exp f v -> InterpM f v (AtVal f v)
int = undefined
