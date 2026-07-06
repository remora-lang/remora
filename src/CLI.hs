module CLI (main) where

import CLI.REPL qualified
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parser qualified
import Pipeline qualified
import System.Console.CmdArgs hiding (args)
import System.Console.CmdArgs qualified as CmdArgs
import System.FilePath (dropExtension, takeFileName, (</>))
import System.IO
import System.Process
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
        expr :: Maybe String,
        args :: [String]
      }
  | Futhark
      { file :: Maybe FilePath,
        expr :: Maybe String,
        backend :: Maybe FutharkBackend
      }
  | Parse
      { file :: Maybe FilePath,
        expr :: Maybe String
      }
  | Monomorphize
      { file :: Maybe FilePath,
        expr :: Maybe String
      }
  deriving (Data, Typeable, Show, Eq)

parse :: RemoraMode
parse =
  Parse
    { file = Nothing &= help "Parse the passed file.",
      expr = Nothing &= help "Parse an expression passed as an argument."
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
      expr = Nothing &= help "Interpret an expression passed as an argument.",
      args = [] &= CmdArgs.args
    }
    &= details
      [ "Interpret a remora program or expression.",
        "",
        "If neither -f nor -e is passed, will read input from stdin."
      ]

monomorphize :: RemoraMode
monomorphize =
  Monomorphize
    { file = Nothing &= help "Monomorphize the passed file.",
      expr = Nothing &= help "Monomorphize an expression passed as an argument."
    }
    &= details
      [ "Monomorphize a remora program or expression.",
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
      [ REPL,
        interpret,
        futhark,
        parse,
        monomorphize
      ]
      &= program "remora"

main :: IO ()
main = do
  passed_mode <- cmdArgsRun mode
  case passed_mode of
    REPL -> CLI.REPL.repl
    Interpret mfile mexpr margs -> do
      input <- handleInput mfile mexpr
      let m = do
            argVals <- mapM evalArg margs
            expr <- doParse mfile input
            Pipeline.interpret argVals expr
      case m of
        Left err -> T.putStrLn err
        Right v -> T.putStrLn $ prettyText v
    Monomorphize mfile mexpr -> do
      input <- handleInput mfile mexpr
      let m = do
            expr <- doParse mfile input
            Pipeline.monomorphize expr
      case m of
        Left err -> T.putStrLn err
        Right e -> T.putStrLn $ prettyText e
    Futhark mfile mexpr mbackend -> do
      input <- handleInput mfile mexpr
      let m = do
            expr <- doParse mfile input
            Pipeline.compile expr
      case m of
        Left err -> T.putStrLn err
        Right v ->
          case mbackend of
            Nothing -> T.putStrLn $ prettyText v
            Just backend -> do
              res <-
                futharkCompile
                  backend
                  (takeFileName $ dropExtension $ fromMaybe "<cli>" mfile)
                  v
              putStrLn res
    Parse mfile mexpr -> do
      input <- handleInput mfile mexpr
      case doParse mfile input of
        Left err -> T.putStrLn err
        Right expr -> T.putStrLn $ prettyText expr
  where
    handleInput :: Maybe FilePath -> Maybe String -> IO Text
    handleInput (Just path) _ = T.readFile path
    handleInput Nothing (Just s) = pure $ T.pack s
    handleInput Nothing Nothing = T.getContents

    doParse mfile = Parser.parse (fromMaybe "<cli>" mfile)

    evalArg s =
      Parser.parseExp "<arg>" (T.pack s) >>= Pipeline.interpretExp

    futharkCompile :: FutharkBackend -> String -> Text -> IO String
    futharkCompile backend fname ir = do
      let src = "." </> (fname <> ".fut_soacs")
      T.writeFile src ir
      readProcess
        "futhark"
        ( ["dev"]
            <> backendArgs backend
            <> [src]
        )
        []
      where
        backendArgs C =
          [ "--seq-mem",
            "--backend=c"
          ]
        backendArgs CUDA =
          [ "--gpu-mem",
            "--backend=cuda"
          ]
