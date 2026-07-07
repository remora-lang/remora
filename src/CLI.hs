module CLI (main) where

import CLI.REPL qualified
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Imports qualified
import Parser qualified
import Pass (runPassIO)
import Pipeline qualified
import Syntax
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
  either T.putStrLn pure =<< runExceptT (run passed_mode)
  where
    run :: RemoraMode -> ExceptT Error IO ()
    run REPL = liftIO CLI.REPL.repl
    run (Interpret mfile mexpr margs) = do
      input <- parseInput mfile mexpr
      v <-
        except $
          either
            Pipeline.interpretExp
            (\prog -> flip Pipeline.interpret prog =<< mapM evalArg margs)
            input
      liftIO $ T.putStrLn $ prettyText v
    run (Monomorphize mfile mexpr) = do
      input <- parseInput mfile mexpr
      out <-
        except $
          either
            (fmap prettyText . Pipeline.monomorphizeExp)
            (fmap prettyText . Pipeline.monomorphize)
            input
      liftIO $ T.putStrLn out
    run (Futhark mfile mexpr mbackend) = do
      input <- parseInput mfile mexpr
      ir <- except $ either Pipeline.compileExp Pipeline.compile input
      case mbackend of
        Nothing -> liftIO $ T.putStrLn $ prettyText ir
        Just backend ->
          liftIO $
            putStrLn
              =<< futharkCompile
                backend
                (takeFileName $ dropExtension $ sourceName mfile)
                ir
    run (Parse mfile mexpr) = do
      input <- parseInput mfile mexpr
      liftIO $ T.putStrLn $ either prettyText prettyText input

    parseInput ::
      Maybe FilePath ->
      Maybe String ->
      ExceptT Error IO (Either UncheckedExp UncheckedProg)
    parseInput Nothing (Just s) =
      Left <$> except (Parser.parseExp (sourceName Nothing) (T.pack s))
    parseInput mfile _ = do
      input <- liftIO $ handleInput mfile
      Right
        <$> ExceptT
          (runPassIO $ Imports.resolveImports (sourceName mfile) input)

    sourceName :: Maybe FilePath -> FilePath
    sourceName = fromMaybe "<cli>"

    handleInput :: Maybe FilePath -> IO Text
    handleInput (Just path) = T.readFile path
    handleInput Nothing = T.getContents

    evalArg s =
      Pipeline.interpretExp
        =<< Parser.parseExp "<arg>" (T.pack s)

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
