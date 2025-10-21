{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CLI (main) where

import CLI.REPL qualified
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Interpreter qualified
import Interpreter.Value (Val)
import Parser qualified
import Prettyprinter
import SExp
import Syntax
import System.Console.CmdArgs
import TypeCheck
import Util
import Prelude hiding (exp)

data Remora
  = REPL
  | Interpret
      { file :: Maybe FilePath,
        expr :: Maybe String
      }
  | Parse
      { file :: Maybe FilePath,
        expr :: Maybe String,
        sexp :: Bool
      }
  deriving (Data, Typeable, Show, Eq)

parse =
  Parse
    { file = Nothing &= help "Parse the passed file.",
      expr = Nothing &= help "Parse an expression passed as an argument.",
      sexp = False &= help "Print the parsed result as an s-expression."
    }
    &= details
      [ "Parse a Remora program or expression.",
        "",
        "Expressions may be passed directly as an argument using the -e flag, e.g.:",
        "> remora parse -e \"[[1 2] [3 4]]\"",
        "",
        "If neither -f nor -e is passed, will read input from stdin."
      ]

interpret =
  Interpret
    { file = Nothing &= help "Interpret the passed file.",
      expr = Nothing &= help "Interpret an expression passed as an argument."
    }
    &= details
      [ "Interpret a Remora program or expression.",
        "",
        "Expressions may be passed directly as an argument using the -e flag, e.g.:",
        "> remora interpret -e \"[[1 2] [3 4]]\"",
        "",
        "If neither -f nor -e is passed, will read input from stdin."
      ]

mode =
  cmdArgsMode $
    modes
      [ REPL &= details ["DO NOT USE: remora repl is a broken WIP."],
        interpret,
        parse
      ]
      &= program "remora"

main :: IO ()
main = do
  mode <- cmdArgsRun mode
  case mode of
    REPL -> CLI.REPL.repl
    Interpret mfile mexpr -> do
      input <- handleInput mfile mexpr
      let m = do
            expr <- doParse mfile input
            (prelude, expr') <- check expr
            Interpreter.interpret prelude expr'
      case m of
        Left err -> T.putStrLn err
        Right v -> T.putStrLn $ prettyText v
    Parse mfile mexpr sexp -> do
      input <- handleInput mfile mexpr
      case doParse mfile input of
        Left err -> T.putStrLn err
        Right expr
          | sexp -> T.putStrLn $ prettyText (toSExp expr :: SExp Text)
          | otherwise -> T.putStrLn $ prettyText expr
    _ -> print "Unsupported."
  where
    handleInput :: Maybe FilePath -> Maybe String -> IO Text
    handleInput (Just path) _ = T.readFile path
    handleInput Nothing (Just s) = pure $ T.pack s
    handleInput Nothing Nothing = T.getContents

    doParse mfile input = Parser.parse (fromMaybe "<cli>" mfile) input
