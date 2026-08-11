{-# OPTIONS_GHC -fno-cse #-}

module CLI (main) where

import CLI.Futhark (FutharkBackend (..), backendOptions)
import CLI.REPL qualified
import CLI.Test qualified
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Data.Aeson (ToJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as B
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Imports qualified
import Parser qualified
import Pass (runPassIO)
import Pipeline qualified
import Prettyprinter (Pretty)
import Serializer ()
import Syntax
import System.Console.CmdArgs hiding (args, modes)
import System.Console.CmdArgs qualified as CmdArgs
import System.Exit (exitFailure)
import System.FilePath (dropExtension, takeFileName, (</>))
import System.IO
import System.Process
import Util
import Prelude hiding (exp)

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
  | Test
      { paths :: [FilePath],
        backend :: Maybe FutharkBackend,
        modes :: [String],
        tags :: [String],
        excludeTags :: [String]
      }
  | Dev
      { file :: Maybe FilePath,
        expr :: Maybe String,
        parse_ :: Bool,
        uniquify :: Bool,
        typeCheck :: Bool,
        lambdalift :: Bool,
        monomorphize :: Bool,
        json :: Bool
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

test :: RemoraMode
test =
  Test
    { paths = [] &= CmdArgs.args &= typ "FILES",
      backend =
        Nothing
          &= name "backend"
          &= help "Backend used by compiled tests (c|cuda). Defaults to c."
          &= typ "c|cuda",
      modes =
        []
          &= explicit
          &= name "modes"
          &= help "Run tests in these modes, ignoring their modes directive."
          &= typ "interpret|compile",
      tags =
        []
          &= explicit
          &= name "tags"
          &= help "Only run tests carrying one of these tags."
          &= typ "TAGS",
      excludeTags =
        []
          &= explicit
          &= name "exclude-tags"
          &= help "Skip tests carrying any of these tags."
          &= typ "TAGS"
    }
    &= details
      [ "Run the test blocks of the passed Remora files.",
        "",
        "Tests are interpreted and compiled unless their modes directive says",
        "otherwise. Passing --modes overrides every test, e.g. to interpret all",
        "of them:",
        "> remora test --modes interpret tests/",
        "",
        "The --modes, --tags and --exclude-tags flags each accept several values",
        "separated by spaces, and may be passed more than once."
      ]

dev :: RemoraMode
dev =
  Dev
    { file = Nothing &= help "Run the pass on the passed file.",
      expr = Nothing &= help "Run the pass on an expression passed as an argument.",
      parse_ = False &= help "Parse only.",
      uniquify = False &= help "Uniquify and expand array type var syntactic sugar.",
      typeCheck = False &= help "Type check.",
      lambdalift = False &= help "Lambda lift.",
      monomorphize = False &= help "Lambda lift & monomorphize.",
      json = False &= help "Serialize the result of the pass to JSON."
    }
    &= details
      [ "Run a single compiler pass.",
        "",
        "The optional -j flag determines the output format.",
        "",
        "If neither -f nor -e is passed, will read input from stdin."
      ]

mode :: Mode (CmdArgs RemoraMode)
mode =
  cmdArgsMode $
    CmdArgs.modes
      [ REPL,
        interpret,
        futhark,
        parse,
        test,
        dev
      ]
      &= program "remora"

main :: IO ()
main = do
  passed_mode <- cmdArgsRun mode
  either problem pure =<< runExceptT (run passed_mode)
  where
    problem s = do
      T.hPutStrLn stderr s
      exitFailure

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
    run (Test test_paths mbackend mmodes mtags mexclude) = do
      test_modes <- except $ traverse CLI.Test.readMode $ concatMap words mmodes
      let options =
            CLI.Test.Options
              { CLI.Test.optionsBackend = fromMaybe C mbackend,
                CLI.Test.optionsModes = test_modes,
                CLI.Test.optionsTags = map T.pack $ concatMap words mtags,
                CLI.Test.optionsExcludeTags = map T.pack $ concatMap words mexclude
              }
      failed <- liftIO $ CLI.Test.runTests options test_paths
      when (failed > 0) $ liftIO exitFailure
    run (Dev mfile mexpr pars uniq check lift mono json)
      | pars = devPass pure pure
      | uniq = devPass Pipeline.uniquifyExp Pipeline.uniquify
      | check = devPass Pipeline.typeCheckExp Pipeline.typeCheck
      | lift = devPass Pipeline.lambdaLiftExp Pipeline.lambdaLift
      | mono = devPass Pipeline.monomorphizeExp Pipeline.monomorphize
      | otherwise =
          except $
            Left
              "remora dev: specify --parse, --uniquify, --typecheck, --lambdalift or --monomorphize"
      where
        devPass ::
          (Pretty a, ToJSON a, Pretty b, ToJSON b) =>
          (UncheckedExp -> Either Error a) ->
          (UncheckedProg -> Either Error b) ->
          ExceptT Error IO ()
        devPass onExp onProg = do
          input <- parseInput mfile mexpr
          out <-
            except $
              either
                (fmap putResult . onExp)
                (fmap putResult . onProg)
                input
          liftIO out

        putResult ::
          (Pretty a, ToJSON a) =>
          a ->
          IO ()
        putResult
          | json = B.putStr . (<> "\n") . encodePretty
          | otherwise = T.putStrLn . prettyText

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
      liftIO $ either (problem . prettyText) (T.putStrLn . prettyText) input

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
            <> backendOptions backend
            <> [src]
        )
        []
