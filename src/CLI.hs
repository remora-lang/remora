{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module CLI (main) where

import CLI.REPL qualified
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark qualified
import Interpreter qualified
import Parser qualified
import SExp
import System.Console.CmdArgs
import System.FilePath (dropExtension, takeFileName, (</>))
import System.IO
import System.Process
import TypeCheck
import Util
import Prelude hiding (exp)

data FutharkBackend
  = C
  | CUDA
  deriving (Data, Typeable, Show, Eq)

data RemoraMode
  = REPL
  | Interpret
      { file :: Maybe FilePath,
        expr :: Maybe String
      }
  | Futhark
      { file :: Maybe FilePath,
        expr :: Maybe String,
        backend :: Maybe FutharkBackend
      }
  | Parse
      { file :: Maybe FilePath,
        expr :: Maybe String,
        sexp :: Bool
      }
  deriving (Data, Typeable, Show, Eq)

parse :: RemoraMode
parse =
  Parse
    { file = Nothing &= help "Parse the passed file.",
      expr = Nothing &= help "Parse an expression passed as an argument.",
      sexp = False &= help "Print the parsed result as an s-expression."
    }
    &= details
      [ "Parse a remora program or expression.",
        "",
        "Expressions may be passed directly as an argument using the -e flag, e.g.:",
        "> remora parse -e \"[[1 2] [3 4]]\"",
        "",
        "If neither -f nor -e is passed, will read input from stdin."
      ]

interpret :: RemoraMode
interpret =
  Interpret
    { file = Nothing &= help "Interpret the passed file.",
      expr = Nothing &= help "Interpret an expression passed as an argument."
    }
    &= details
      [ "Interpret a remora program or expression.",
        "",
        "If neither -f nor -e is passed, will read input from stdin."
      ]

futhark :: RemoraMode
futhark =
  Futhark
    { file = Nothing &= help "Turn the passed file into Futhark.",
      expr = Nothing &= help "Turn the passed expression into Futhark.",
      backend =
        Nothing
          &= name "backend"
          &= help "Emit backend code (c|cuda). If omitted, print Futhark IR."
          &= typ "c|cuda"
    }
    &= details
      [ "Turn a Remora program into Futhark.",
        "",
        "If neither -f nor -e is passed, will read input from stdin."
      ]

mode :: Mode (CmdArgs RemoraMode)
mode =
  cmdArgsMode $
    modes
      [ REPL &= details ["DO NOT USE: remora repl is a broken WIP."],
        interpret,
        futhark,
        parse
      ]
      &= program "remora"

main :: IO ()
main = do
  passed_mode <- cmdArgsRun mode
  case passed_mode of
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
    Futhark mfile mexpr mbackend -> do
      input <- handleInput mfile mexpr
      let m = do
            expr <- doParse mfile input
            (prelude, expr') <- check expr
            Futhark.compile prelude expr'
      case m of
        Left err -> T.putStrLn err
        Right v ->
          case mbackend of
            Nothing -> T.putStrLn $ prettyText v
            Just backend -> do
              res <- futharkCompile backend (takeFileName $ dropExtension $ fromMaybe "<cli>" mfile) v
              putStrLn res
    Parse mfile mexpr sexp -> do
      input <- handleInput mfile mexpr
      case doParse mfile input of
        Left err -> T.putStrLn err
        Right expr
          | sexp -> T.putStrLn $ prettyText (toSExp expr :: SExp Text)
          | otherwise -> T.putStrLn $ prettyText expr
  where
    handleInput :: Maybe FilePath -> Maybe String -> IO Text
    handleInput (Just path) _ = T.readFile path
    handleInput Nothing (Just s) = pure $ T.pack s
    handleInput Nothing Nothing = T.getContents

    doParse mfile = Parser.parse (fromMaybe "<cli>" mfile)

    futharkCompile :: FutharkBackend -> String -> Text -> IO String
    futharkCompile backend fname ir = do
      let src = "." </> (fname <> ".fut_soacs")
      T.writeFile src ir
      readProcess
        "futhark"
        ( ["dev"]
            <> args backend
            <> [src]
        )
        []
      where
        args C =
          [ "--seq-mem",
            "--backend=c"
          ]
        args CUDA =
          [ "--gpu-mem",
            "--backend=cuda"
          ]
