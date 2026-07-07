module Imports (resolveImports) where

import Control.Monad.Error.Class (liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text.IO qualified as T
import Parser qualified
import Pass
import Syntax
import System.Directory (makeAbsolute)
import System.FilePath (takeDirectory, (</>))

resolveImports :: (MonadIO m) => FilePath -> Text -> PassT m UncheckedProg
resolveImports fname input = evalStateT (resolve fname input) mempty

resolve ::
  (MonadIO m) =>
  FilePath ->
  Text ->
  StateT (Set FilePath) (PassT m) UncheckedProg
resolve fname input = do
  (imports, prog) <- lift $ liftEither $ Parser.parse fname input
  dss <- traverse (resolveImport fname) imports
  pure $ Prog $ concat dss <> progDecs prog

resolveImport ::
  (MonadIO m) =>
  FilePath ->
  Import ->
  StateT (Set FilePath) (PassT m) [UncheckedDecl]
resolveImport importer (Import path _) = do
  seen <- get
  path' <- liftIO $ makeAbsolute $ takeDirectory importer </> path
  if path' `S.member` seen
    then pure mempty
    else do
      modify $ S.insert path'
      contents <- liftIO $ T.readFile path'
      progDecs <$> resolve path' contents
