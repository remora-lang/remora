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

import CLI.Futhark (FutharkBackend, Input (..), compileAndRun, futharkInput, futharkValue)
import CLI.Test.Parser
import Control.Exception (ErrorCall (..), evaluate, try)
import Control.Monad (forM, when)
import Data.ByteString.Lazy qualified as LBS
import Data.Foldable (for_)
import Data.List (isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Futhark.Data.Reader qualified as F
import GHC.IO.Exception (IOException (..))
import Imports qualified
import Interpreter qualified
import Pass (runPassIO)
import Pipeline qualified
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (<.>), (</>))
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

data Checked
  = CheckPassed
  | CheckFailed Text Text
  | CheckDumped Text Text

maxInlineLength :: Int
maxInlineLength = 1000

runTest :: Options -> FilePath -> Text -> Text -> Maybe Int -> TestRun -> Mode -> IO Outcome
runTest options path source entry index run mode = do
  resolved <- case (mode, runInput run) of
    (Compile, InFile file) -> pure $ Right $ InputFile $ dir </> file
    (_, values) -> fmap InputValues <$> resolveValues dir values
  expected <- traverse (resolveExpected dir) $ runExpected run
  case (,) <$> resolved <*> sequence expected of
    Left err -> pure $ Failed err
    Right (input, wanted) -> do
      prog <- runPassIO $ Imports.resolveImports path source
      checked <- try $ evaluate . forced . check wanted =<< runMode prog input
      case checked of
        Left (ErrorCall message) -> pure $ Failed $ T.pack message
        Right result -> report result
  where
    dir = takeDirectory path

    forced CheckPassed = CheckPassed
    forced outcome@(CheckFailed expected actual) = expected `seq` actual `seq` outcome
    forced outcome@(CheckDumped expected actual) = expected `seq` actual `seq` outcome

    runMode prog (InputValues values)
      | mode == Interpret = pure $ Pipeline.interpret entry values =<< prog
    runMode prog input =
      case prog >>= Pipeline.compile of
        Left err -> pure $ Left err
        Right ir ->
          compileAndRun (optionsBackend options) (takeBaseName path) ir entry input

    check Nothing = checkSuccess
    check (Just (ExpectError pattern regex)) = checkError pattern regex
    check (Just (ExpectOutput expected)) = checkOutput expected

    checkSuccess (Left err) = CheckFailed "success" err
    checkSuccess (Right value) = prettyText value `seq` CheckPassed

    checkError pattern regex (Left err)
      | match regex $ T.unpack err = CheckPassed
      | otherwise = CheckFailed ("error matching " <> pattern) err
    checkError pattern _ (Right value) =
      CheckFailed ("error matching " <> pattern) $ prettyText value

    checkOutput expected (Left err) = CheckFailed (prettyText expected) err
    checkOutput expected (Right value)
      | expected == value = CheckPassed
      | otherwise =
          case (futharkInput [expected], futharkInput [value]) of
            (Right wanted, Right got)
              | T.length wanted + T.length got > maxInlineLength ->
                  CheckDumped wanted got
            _ -> CheckFailed (prettyText expected) (prettyText value)

    report CheckPassed = pure Passed
    report (CheckFailed expected actual) =
      pure $ Failed $ T.unlines ["expected " <> expected <> ", but got:", actual]
    report (CheckDumped expected actual) = do
      written <- try $ T.writeFile expected_path expected >> T.writeFile actual_path actual
      pure $ Failed $ case written of
        Left err -> T.pack $ show (err :: IOException)
        Right () ->
          T.pack actual_path <> " and " <> T.pack expected_path <> " do not match"
      where
        expected_path = dumpPath "expected"
        actual_path = dumpPath "actual"
        dumpPath extension =
          path <.> T.unpack entry <.> maybe extension (\i -> show i <.> extension) index

resolveExpected ::
  FilePath ->
  Expected Values ->
  IO (Either Error (Expected Interpreter.Val))
resolveExpected _ (ExpectError pattern regex) = pure $ Right $ ExpectError pattern regex
resolveExpected dir (ExpectOutput values) = do
  resolved <- resolveValues dir values
  pure $ ExpectOutput <$> (onlyValue =<< resolved)
  where
    onlyValue [value] = Right value
    onlyValue vs =
      Left $ "expected a single output value, but got " <> prettyText (length vs)

resolveValues :: FilePath -> Values -> IO (Either Error [Interpreter.Val])
resolveValues _ (Inline values) = pure $ Right values
resolveValues dir (InFile file) = do
  contents <- try $ LBS.readFile path
  pure $ case contents of
    Left err -> Left $ T.pack $ show (err :: IOException)
    Right bytes ->
      case F.readValues bytes of
        Nothing -> Left $ "cannot read values from " <> T.pack path
        Just values -> traverse futharkValue values
  where
    path = dir </> file

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
                  <$> runTest options path source entry i run mode

selected :: Options -> TestBlock -> Bool
selected options block =
  (null (optionsTags options) || any (`elem` optionsTags options) (testTags block))
    && not (any (`elem` optionsExcludeTags options) (testTags block))
    && ( null (optionsEntries options)
           || entryFor block `elem` optionsEntries options
       )
