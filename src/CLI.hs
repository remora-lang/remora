{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CLI (main) where

import CLI.REPL qualified
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parser qualified
import Prettyprinter
import SExp
import System.Console.CmdArgs
import Util
import Prelude hiding (exp)

data Remora
  = REPL
  | Interpret
  | Parse
      { file :: Maybe FilePath,
        exp :: Maybe String,
        sexp :: Bool
      }
  deriving (Data, Typeable, Show, Eq)

parse =
  Parse
    { file = Nothing &= help "Parse the passed file.",
      exp = Nothing &= help "Parse an expression passed as an argument.",
      sexp = False &= help "Print the parsed result as an s-expression."
    }
    &= details
      [ "Parse a Remora program or expression.",
        "",
        "Expressions may be passed directly as an argument using the -e flag, e.g.:",
        "> remora parse -e \"[[1 2] [3 4]]\"",
        "",
        "If no flags are passed, will read input from stdin."
      ]

mode =
  cmdArgsMode $
    modes
      [ REPL &= details ["DO NOT USE: remora repl is a broken WIP."],
        Interpret &= details ["DO NOT USE: remora interpret is broken WIP."],
        parse
      ]
      &= program "remora"

main :: IO ()
main = do
  mode <- cmdArgsRun mode
  case mode of
    REPL -> CLI.REPL.repl
    Parse mfile mexp sexp -> do
      input <- case mfile of
        Just path -> T.readFile path
        Nothing ->
          case mexp of
            Just s -> pure $ T.pack s
            Nothing -> T.getContents

      case Parser.parse (fromMaybe "<cli>" mfile) input of
        Left err -> putStrLn err
        Right exp
          | sexp -> putStrLn $ prettyString (toSExp exp :: SExp Text)
          | otherwise -> putStrLn $ prettyString exp
    _ -> print "Unsupported."
