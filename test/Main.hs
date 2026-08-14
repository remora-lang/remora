module Main (main) where

import CLI.Futhark (FutharkBackend (..))
import CLI.Test
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.FilePath (takeFileName)
import Test.Tasty
import Test.Tasty.HUnit

testsDir :: FilePath
testsDir = "tests"

options :: Options
options =
  Options
    { optionsBackend = C,
      optionsModes = Nothing,
      optionsEntries = mempty,
      optionsTags = mempty,
      optionsExcludeTags = mempty,
      optionsInterpretExcludeTags = mempty,
      optionsCompileExcludeTags = mempty
    }

main :: IO ()
main = do
  cases <- concat <$> (mapM mkCase =<< testFiles testsDir)
  defaultMain $ testGroup "remora" cases

mkCase :: FilePath -> IO [TestTree]
mkCase path = do
  source <- T.readFile path
  pure $ case testBlocks path source of
    Left err -> [testCase (takeFileName path) $ assertFailure $ T.unpack err]
    Right blocks ->
      [ testCase (T.unpack $ testLabel (takeFileName path) entry i mode) $ do
          outcome <- runTest options path source entry run mode
          case (xfail, outcome) of
            (False, Passed) -> pure ()
            (False, Failed message) -> assertFailure $ T.unpack message
            (True, Failed _) -> pure ()
            (True, Passed) ->
              assertFailure "unexpectedly passed; drop the tag exempting it"
      | block <- filter (selected options) blocks,
        let entry = entryFor block,
        (i, run) <- numberedRuns block,
        mode <- testModesFor options block,
        let xfail =
              mode == Compile
                && any
                  (`elem` ["higher-order", "existential"])
                  (testTags block)
      ]
