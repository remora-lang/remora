module CLI.Test
  ( TestBlock (..),
    Expected (..),
    Mode (..),
    Options (..),
    Outcome (..),
    testBlock,
    readMode,
    testFiles,
    runTest,
    runTests,
  )
where

import CLI.Futhark (FutharkBackend, compileAndRun)
import CLI.Test.Parser
import Control.Exception (ErrorCall (..), evaluate, try)
import Data.Foldable (for_)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.IO.Exception (IOException (..))
import Imports qualified
import Pass (runPassIO)
import Pipeline qualified
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeExtension, (</>))
import Text.Regex.TDFA (match)
import Util

data Options = Options
  { optionsBackend :: FutharkBackend,
    optionsModes :: [Mode],
    optionsTags :: [Text],
    optionsExcludeTags :: [Text]
  }

data Outcome
  = Passed
  | Failed Text

runTest :: Options -> FilePath -> Text -> TestBlock -> IO Outcome
runTest options path source block = do
  prog <- runPassIO $ Imports.resolveImports path source
  combine <$> mapM (checkMode prog) modes
  where
    modes
      | null $ optionsModes options = testModes block
      | otherwise = optionsModes options

    checkMode prog mode = do
      outcome <- try $ evaluate . inMode mode . check =<< runMode prog mode
      pure $ case outcome of
        Left (ErrorCall message) -> inMode mode $ Failed $ T.pack message
        Right result -> result

    runMode prog Interpret = pure $ prog >>= Pipeline.interpret mempty
    runMode prog Compile =
      case prog >>= Pipeline.compile of
        Left err -> pure $ Left err
        Right ir -> compileAndRun (optionsBackend options) (takeBaseName path) ir

    inMode _ Passed = Passed
    inMode mode (Failed message) = Failed $ prettyText mode <> ": " <> message

    check =
      case testExpected block of
        Nothing -> checkSuccess
        Just (ExpectError pattern regex) -> checkError pattern regex
        Just (ExpectOutput expected) -> checkOutput expected

    checkSuccess (Left err) = failed "success" err
    checkSuccess (Right _) = Passed

    checkError pattern regex (Left err)
      | match regex $ T.unpack err = Passed
      | otherwise = failed ("error matching " <> pattern) err
    checkError pattern _ (Right value) =
      failed ("error matching " <> pattern) $ prettyText value

    checkOutput expected (Left err) = failed (prettyText expected) err
    checkOutput expected (Right value)
      | expected == value = Passed
      | otherwise = failed (prettyText expected) $ prettyText value

    failed expected actual =
      Failed $ "expected " <> expected <> ", but got:\n" <> actual

combine :: [Outcome] -> Outcome
combine outcomes =
  case [message | Failed message <- outcomes] of
    [] -> Passed
    messages -> Failed $ T.intercalate "\n" messages

runTests :: Options -> [FilePath] -> IO Int
runTests options paths = do
  files <- concat <$> mapM testFiles paths
  outcomes <- catMaybes <$> mapM (runFile options) files
  let failures = [(path, message) | (path, Failed message) <- outcomes]
      failed = length failures
  for_ failures $ \(path, message) ->
    T.putStrLn $ T.pack path <> ":\n" <> message <> "\n"
  T.putStrLn $
    prettyText (length outcomes - failed) <> " passed, " <> prettyText failed <> " failed"
  pure failed

testFiles :: FilePath -> IO [FilePath]
testFiles path = do
  directory <- doesDirectoryExist path
  if not directory
    then pure [path]
    else do
      entries <- sort . filter (not . ("." `isPrefixOf`)) <$> listDirectory path
      concat <$> mapM (entryFiles . (path </>)) entries
  where
    entryFiles entry = do
      directory <- doesDirectoryExist entry
      if directory || takeExtension entry == ".remora"
        then testFiles entry
        else pure []

runFile :: Options -> FilePath -> IO (Maybe (FilePath, Outcome))
runFile options path = do
  source <- try $ T.readFile path
  either unreadable runSource source
  where
    unreadable err =
      pure $ Just (path, Failed $ T.pack $ show err {ioe_filename = Nothing})

    runSource source =
      case testBlock path source of
        Left err -> pure $ Just (path, Failed err)
        Right Nothing -> pure Nothing
        Right (Just block)
          | selected block -> Just . (,) path <$> runTest options path source block
          | otherwise -> pure Nothing

    selected block =
      (null (optionsTags options) || any (`elem` optionsTags options) (testTags block))
        && not (any (`elem` optionsExcludeTags options) (testTags block))
