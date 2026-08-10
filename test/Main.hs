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
      optionsModes = [Interpret], -- TODO: fix once we fix the backend
      optionsTags = mempty,
      optionsExcludeTags = mempty
    }

main :: IO ()
main = do
  cases <- concat <$> (mapM mkCase =<< testFiles testsDir)
  defaultMain $ testGroup "remora" cases

mkCase :: FilePath -> IO [TestTree]
mkCase path = do
  source <- T.readFile path
  pure $ case testBlock path source of
    Left err -> [testCase (takeFileName path) $ assertFailure $ T.unpack err]
    Right Nothing -> []
    Right (Just block) ->
      [ testCase (takeFileName path) $ do
          outcome <- runTest options path source block
          case outcome of
            Passed -> pure ()
            Failed message -> assertFailure $ T.unpack message
      ]
