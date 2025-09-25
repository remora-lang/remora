{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CLI (main) where

import CLI.REPL qualified
import System.Console.CmdArgs

data Remora
  = REPL
  | Interpret
  deriving (Data, Typeable, Show, Eq)

repl = REPL

interpret = Interpret

mode = cmdArgsMode $ modes [repl, interpret] &= program "remora"

main :: IO ()
main = do
  mode <- cmdArgsRun mode
  case mode of
    repl -> CLI.REPL.repl
    _ -> print "Unsupported."
