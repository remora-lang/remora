{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CLI (main) where

import CLI.REPL qualified
import Data.Text qualified as T
import Parser qualified
import Prettyprinter
import System.Console.CmdArgs
import Util
import Prelude hiding (exp)

data Remora
  = REPL
  | Interpret
  | Parse {exp :: String}
  deriving (Data, Typeable, Show, Eq)

mode = cmdArgsMode $ modes [REPL, Interpret, Parse {exp = def}] &= program "remora"

main :: IO ()
main = do
  mode <- cmdArgsRun mode
  case mode of
    REPL -> CLI.REPL.repl
    Parse s ->
      case Parser.parse "" $ T.pack s of
        Left err -> putStrLn err
        Right exp -> putStrLn $ prettyString exp
    _ -> print "Unsupported."
