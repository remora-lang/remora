module CLI.Test.Parser
  ( TestBlock (..),
    Expected (..),
    Mode (..),
    testBlock,
    readMode,
  )
where

import Control.Monad (unless, void)
import Control.Monad.Permutations
import Data.Char (isAlphaNum)
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
    testExpected :: Maybe Expected
  }

data Expected
  = ExpectOutput Interpreter.Val
  | ExpectError Text Regex

data Mode
  = Interpret
  | Compile
  deriving (Eq, Show)

instance Pretty Mode where
  pretty Interpret = "interpret"
  pretty Compile = "compile"

type Parser = Parsec Void Text

testBlock :: FilePath -> Text -> Either Error (Maybe TestBlock)
testBlock = parseWith $ optional pTestBlock

readMode :: String -> Either Error Mode
readMode = parseWith (spaceConsumer *> pMode <* eof) "<mode>" . T.pack

parseWith :: Parser a -> FilePath -> Text -> Either Error a
parseWith p fname s =
  case parse p fname s of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right x -> Right x

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lCommentStart empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

directives :: [Text]
directives = ["tags", "modes", "output", "error"]

keywords :: Set Text
keywords = S.fromList $ directives <> ["interpret", "compile"]

lKeyword :: Text -> Parser ()
lKeyword s
  | s `S.member` keywords =
      lexeme $ try $ void $ string s <* notFollowedBy (satisfy isAlphaNum)
  | otherwise = fail $ "not a keyword: " <> T.unpack s

lCommentStart :: Parser ()
lCommentStart = skipSome $ char ';'

lRestOfLine :: Parser Text
lRestOfLine = lexeme restOfLine

restOfLine :: Parser Text
restOfLine = T.strip . T.pack <$> many (satisfy (/= '\n'))

pTestBlock :: Parser TestBlock
pTestBlock = do
  description <- try $ space *> manyTill lCommentLine pDivider
  spaceConsumer
  block <-
    runPermutation $
      TestBlock (T.strip $ T.unlines description)
        <$> toPermutationWithDefault mempty pTags
        <*> toPermutationWithDefault [Interpret, Compile] pModes
        <*> toPermutationWithDefault Nothing (Just <$> pResult)
  block <$ noRepeatedDirective
  where
    noRepeatedDirective :: Parser ()
    noRepeatedDirective = notFollowedBy $ choice $ map lKeyword directives

    pDivider :: Parser ()
    pDivider = try $ do
      line <- lCommentLine
      unless (line == "==") $ fail $ "not a divider: " <> T.unpack line

    lCommentLine :: Parser Text
    lCommentLine = hspace *> lCommentStart *> restOfLine <* (void eol <|> eof)

pTags :: Parser [Text]
pTags = lKeyword "tags" >> braces (many lTag)
  where
    lTag :: Parser Text
    lTag = lexeme $ T.pack <$> some (satisfy $ \c -> isAlphaNum c || c `elem` ['-', '_'])

pModes :: Parser [Mode]
pModes = lKeyword "modes" >> braces (some pMode)

pMode :: Parser Mode
pMode =
  choice
    [ Interpret <$ lKeyword "interpret",
      Compile <$ lKeyword "compile"
    ]

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
