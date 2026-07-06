module Main (main) where

import Data.Char (isSpace)
import Data.List (isPrefixOf, sort, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as T
import Parser qualified
import Pipeline qualified
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit
import Util (prettyString)

-- | A super basic test harnness to serve in the interim before we get `remora
-- test`.

-- | Directory scanned for @.remora@ test programs.
testsDir :: FilePath
testsDir = "tests"

main :: IO ()
main = do
  files <- listDirectory testsDir
  let remoraFiles =
        sort [f | f <- files, takeExtension f == ".remora", not ("." `isPrefixOf` f)]
  cases <- concat <$> mapM mkCase remoraFiles
  defaultMain $ testGroup "remora" cases

-- | Test files must contain a @;; expected:@ line.
mkCase :: FilePath -> IO [TestTree]
mkCase name = do
  let path = testsDir </> name
  src <- readFile path
  pure $ case expectedValue src of
    Nothing -> []
    Just expected ->
      [ testCase name $ case Parser.parse path (T.pack src) >>= Pipeline.interpret mempty of
          Left err ->
            assertFailure $ "evaluation failed:\n" <> T.unpack err
          Right val ->
            let actual = prettyString val
             in assertBool
                  ("expected: " <> expected <> "\nbut got:  " <> actual)
                  (normalize actual == canonical expected)
      ]

-- | When the expected valuable is valid Remora syntax, interpret it.
canonical :: String -> String
canonical expected =
  case Parser.parseExp "<expected>" (T.pack expected) >>= Pipeline.interpretExp of
    Right val -> normalize $ prettyString val
    Left _ -> normalize expected

expectedValue :: String -> Maybe String
expectedValue = listToMaybe . mapMaybe parseLine . lines
  where
    parseLine l = trim <$> stripPrefix ";; expected:" (dropWhile isSpace l)

normalize :: String -> String
normalize = concat . words

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace
