module CLI (main) where

import CLI.REPL qualified
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, modify)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Parser qualified
import Pipeline qualified
import Syntax
import System.Console.CmdArgs hiding (args)
import System.Console.CmdArgs qualified as CmdArgs
import System.Directory (makeAbsolute)
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))
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
      prog <- parseInput mfile mexpr
      argVals <- except $ mapM evalArg margs
      v <- except $ Pipeline.interpret argVals prog
      liftIO $ T.putStrLn $ prettyText v
    run (Monomorphize mfile mexpr) = do
      prog <- parseInput mfile mexpr
      e <- except $ Pipeline.monomorphize prog
      liftIO $ T.putStrLn $ prettyText e
    run (Futhark mfile mexpr mbackend) = do
      prog <- parseInput mfile mexpr
      v <- except $ Pipeline.compile prog
      case mbackend of
        Nothing -> liftIO $ T.putStrLn $ prettyText v
        Just backend ->
          liftIO $
            putStrLn
              =<< futharkCompile
                backend
                (takeFileName $ dropExtension $ fromMaybe "<cli>" mfile)
                v
    run (Parse mfile mexpr) = do
      prog <- parseInput mfile mexpr
      liftIO $ T.putStrLn $ prettyText prog

    parseInput :: Maybe FilePath -> Maybe String -> ExceptT Error IO UncheckedProg
    parseInput mfile mexpr =
      parseWithImports mfile
        =<< liftIO (handleInput mfile mexpr)

    handleInput :: Maybe FilePath -> Maybe String -> IO Text
    handleInput (Just path) _ = T.readFile path
    handleInput Nothing (Just s) = pure $ T.pack s
    handleInput Nothing Nothing = T.getContents

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

parseWithImports :: Maybe FilePath -> Text -> ExceptT Error IO UncheckedProg
parseWithImports mfile =
  flip evalStateT mempty
    . parseWithImports' (fromMaybe "<cli>" mfile)
  where
    parseWithImports' ::
      FilePath ->
      Text ->
      StateT (Set FilePath) (ExceptT Error IO) UncheckedProg
    parseWithImports' fname input = do
      (imports, prog) <- lift $ except $ Parser.parse fname input
      resolveImports fname imports prog

    resolveImports ::
      FilePath ->
      [Import] ->
      UncheckedProg ->
      StateT (Set FilePath) (ExceptT Error IO) UncheckedProg
    resolveImports importer imports prog = do
      dss <- traverse (resolveImport importer) imports
      pure $ Prog $ concat dss <> progDecs prog

    resolveImport ::
      FilePath ->
      Import ->
      StateT (Set FilePath) (ExceptT Error IO) [UncheckedDecl]
    resolveImport importer (Import path _) = do
      seen <- get
      path' <- liftIO $ makeAbsolute $ takeDirectory importer </> path
      if path' `S.member` seen
        then pure mempty
        else do
          modify $ S.insert path'
          contents <- liftIO $ T.readFile path'
          progDecs <$> parseWithImports' path' contents
