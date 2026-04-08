module RemoraPrelude where

import Data.Text
import Interpreter.Value
import Syntax

type Prelude v m = [PreludeVal v m]

data PreludeVal v m = PreludeVal v (ArrayType v) (Val m)

prelude :: (Monad m) => Prelude Text m
