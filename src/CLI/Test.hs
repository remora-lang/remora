module CLI.Test
  ( TestBlock (..),
    Expected (..),
    Mode (..),
    allModes,
    Options (..),
    Outcome (..),
    testBlocks,
    testModesFor,
    numberedRuns,
    entryFor,
    defaultEntry,
    testLabel,
    selected,
    readMode,
    testFiles,
    runTest,
    runTests,
  )
where

import CLI.Futhark (FutharkBackend, compileAndRun)
import CLI.Test.Parser
import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Monad (forM, when)
import Data.Foldable (for_)
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe)
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
    optionsModes :: Maybe [Mode],
    optionsEntries :: [Text],
    optionsTags :: [Text],
    optionsExcludeTags :: [Text],
    optionsInterpretExcludeTags :: [Text],
    optionsCompileExcludeTags :: [Text]
  }

modeExcludeTags :: Options -> Mode -> [Text]
modeExcludeTags options Interpret = optionsInterpretExcludeTags options
modeExcludeTags options Compile = optionsCompileExcludeTags options

data Outcome
  = Passed
  | Failed Text

testModesFor :: Options -> TestBlock -> [Mode]
testModesFor options block = filter (not . excluded) requested
  where
    requested = fromMaybe (testModes block) $ optionsModes options

    excluded mode =
      any (`elem` modeExcludeTags options mode) $ testTags block

numberedRuns :: TestBlock -> [(Maybe Int, TestRun)]
numberedRuns block =
  case testRuns block of
    [run] -> [(Nothing, run)]
    runs -> zip (Just <$> [1 ..]) runs

defaultEntry :: Text
defaultEntry = "main"

entryFor :: TestBlock -> Text
entryFor = fromMaybe defaultEntry . testEntry

testLabel :: FilePath -> Text -> Maybe Int -> Mode -> Text
testLabel path entry run mode =
  T.pack path
    <> ":"
    <> entry
    <> maybe mempty (\i -> " (run " <> prettyText i <> ")") run
    <> " ["
    <> prettyText mode
    <> "]"

runTest :: Options -> FilePath -> Text -> Text -> TestRun -> Mode -> IO Outcome
runTest options path source entry run mode = do
  prog <- runPassIO $ Imports.resolveImports path source
  outcome <- try $ evaluate . forced . check =<< runMode prog mode
  pure $ case outcome of
    Left (ErrorCall message) -> Failed $ T.pack message
    Right result -> result
  where
    forced Passed = Passed
    forced outcome@(Failed message) = message `seq` outcome

    runMode prog Interpret =
      pure $ prog >>= Pipeline.interpret entry (runInput run)
    runMode prog Compile =
      case prog >>= Pipeline.compile of
        Left err -> pure $ Left err
        Right ir ->
          compileAndRun (optionsBackend options) (takeBaseName path) ir entry $
            runInput run

    check =
      case runExpected run of
        Nothing -> checkSuccess
        Just (ExpectError pattern regex) -> checkError pattern regex
        Just (ExpectOutput expected) -> checkOutput expected

    checkSuccess (Left err) = failed "success" err
    checkSuccess (Right value) = prettyText value `seq` Passed

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
      Failed $ T.unlines ["expected " <> expected <> ", but got:", actual]

data Result
  = FileFailed FilePath Text
  | ModeResult FilePath Text (Maybe Int) Mode Outcome

runTests :: Options -> [FilePath] -> IO Int
runTests options paths = do
  files <- concat <$> mapM testFiles paths
  results <- concat <$> mapM (runFile options) files
  for_ results report
  for_ allModes $ \mode ->
    case [outcome | ModeResult _ _ _ mode' outcome <- results, mode' == mode] of
      [] -> pure ()
      outcomes -> T.putStrLn $ prettyText mode <> ": " <> tally outcomes
  let unloadable = length [() | FileFailed {} <- results]
  when (unloadable > 0) $
    T.putStrLn $
      prettyText unloadable <> " file(s) could not be loaded"
  pure $ unloadable + length [() | ModeResult _ _ _ _ (Failed _) <- results]
  where
    report (FileFailed path message) =
      T.putStr $ T.unlines [T.pack path <> ":", message]
    report (ModeResult path entry run mode (Failed message)) =
      T.putStr $ T.unlines [testLabel path entry run mode <> ":", message]
    report ModeResult {} = pure ()

    tally outcomes =
      prettyText (length [() | Passed <- outcomes])
        <> " passed, "
        <> prettyText (length [() | Failed _ <- outcomes])
        <> " failed"

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

runFile :: Options -> FilePath -> IO [Result]
runFile options path = do
  source <- try $ T.readFile path
  either unreadable runSource source
  where
    unreadable err =
      pure [FileFailed path $ T.pack $ show (err {ioe_filename = Nothing})]

    runSource source =
      case testBlocks path source of
        Left err -> pure [FileFailed path err]
        Right blocks ->
          fmap concat $ forM (filter (selected options) blocks) $ \block -> do
            let entry = entryFor block
            fmap concat $ forM (numberedRuns block) $ \(i, run) ->
              forM (testModesFor options block) $ \mode ->
                ModeResult path entry i mode
                  <$> runTest options path source entry run mode

selected :: Options -> TestBlock -> Bool
selected options block =
  (null (optionsTags options) || any (`elem` optionsTags options) (testTags block))
    && not (any (`elem` optionsExcludeTags options) (testTags block))
    && ( null (optionsEntries options)
           || entryFor block `elem` optionsEntries options
       )
