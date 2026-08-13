module CLI.Test.Parser
  ( TestBlock (..),
    TestRun (..),
    Expected (..),
    Mode (..),
    allModes,
    testBlocks,
    readMode,
  )
where

import Control.Monad (unless, void)
import Control.Monad.Permutations
import Data.Char (isAlphaNum)
import Data.List (groupBy)
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Interpreter qualified
import Interpreter.Value.Parser (pVal)
import Prettyprinter (Pretty (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Regex.TDFA
  ( CompOption (..),
    Regex,
    defaultCompOpt,
    defaultExecOpt,
    makeRegexOptsM,
  )
import Util

data TestBlock = TestBlock
  { testDescription :: Text,
    testTags :: [Text],
    testModes :: [Mode],
    testRuns :: [TestRun]
  }

data TestRun = TestRun
  { runInput :: [Interpreter.Val],
    runExpected :: Maybe Expected
  }

data Expected
  = ExpectOutput Interpreter.Val
  | ExpectError Text Regex

data Mode
  = Interpret
  | Compile
  deriving (Eq, Show, Enum, Bounded)

allModes :: [Mode]
allModes = [minBound .. maxBound]

instance Pretty Mode where
  pretty Interpret = "interpret"
  pretty Compile = "compile"

type Parser = Parsec Void Text

testBlocks :: FilePath -> Text -> Either Error [TestBlock]
testBlocks fname source =
  mapM (parseWith pTestBlock fname . blanked) $
    filter (any (isDivider . snd)) commentGroups
  where
    numbered = zip [0 :: Int ..] $ T.lines source

    commentGroups =
      filter (all (isComment . snd)) $
        groupBy (\x y -> isComment (snd x) == isComment (snd y)) numbered

    isComment = T.isPrefixOf ";" . T.stripStart
    isDivider = (== divider) . T.strip . uncomment
    uncomment = T.map $ \c -> if c == ';' then ' ' else c

    blanked group =
      T.unlines [if i `S.member` block then uncomment l else "" | (i, l) <- numbered]
      where
        block = S.fromList $ map fst group

readMode :: String -> Either Error Mode
readMode = parseWith (space *> pMode <* eof) "<mode>" . T.pack

parseWith :: Parser a -> FilePath -> Text -> Either Error a
parseWith p fname s =
  case parse p fname s of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right x -> Right x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

divider :: Text
divider = "=="

directives :: [Text]
directives = ["tags", "modes", "input", "output", "error"]

keywords :: Set Text
keywords = S.fromList $ directives <> map prettyText allModes

lKeyword :: Text -> Parser ()
lKeyword s
  | s `S.member` keywords =
      lexeme $ try $ void $ string s <* notFollowedBy (satisfy isAlphaNum)
  | otherwise = fail $ "not a keyword: " <> T.unpack s

lRestOfLine :: Parser Text
lRestOfLine = lexeme restOfLine

restOfLine :: Parser Text
restOfLine = T.strip . T.pack <$> many (satisfy (/= '\n'))

pTestBlock :: Parser TestBlock
pTestBlock = do
  description <- space *> manyTill lLine pDivider
  space
  block <-
    runPermutation $
      TestBlock (T.strip $ T.unlines description)
        <$> toPermutationWithDefault mempty pTags
        <*> toPermutationWithDefault allModes pModes
  runs <- many pRun
  block (if null runs then [TestRun mempty Nothing] else runs)
    <$ noRepeatedDirective
  where
    noRepeatedDirective :: Parser ()
    noRepeatedDirective = notFollowedBy $ choice $ map lKeyword directives

    pDivider :: Parser ()
    pDivider = try $ do
      line <- lLine
      unless (line == divider) $ fail $ "not a divider: " <> T.unpack line

    lLine :: Parser Text
    lLine = restOfLine <* eol

pTags :: Parser [Text]
pTags = lKeyword "tags" >> braces (many lTag)
  where
    lTag :: Parser Text
    lTag = lexeme $ T.pack <$> some (satisfy $ \c -> isAlphaNum c || c `elem` ['-', '_'])

pModes :: Parser [Mode]
pModes = lKeyword "modes" >> braces (some pMode)

pMode :: Parser Mode
pMode = choice [mode <$ lKeyword (prettyText mode) | mode <- allModes]

pRun :: Parser TestRun
pRun =
  choice
    [ TestRun <$> pInput <*> optional pResult,
      TestRun mempty . Just <$> pResult
    ]

pInput :: Parser [Interpreter.Val]
pInput = lKeyword "input" >> braces (many pVal)

pResult :: Parser Expected
pResult =
  choice
    [ ExpectOutput <$> (lKeyword "output" >> braces pVal),
      lKeyword "error" >> char ':' >> pRegex
    ]

pRegex :: Parser Expected
pRegex = do
  written <- lRestOfLine
  let pattern = if T.null written then ".*" else written
  ExpectError pattern
    <$> makeRegexOptsM defaultCompOpt {multiline = False} defaultExecOpt (T.unpack pattern)
