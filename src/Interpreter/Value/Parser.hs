module Interpreter.Value.Parser (parseVal, pVal) where

import Control.Monad (unless, void)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Interpreter.Value
import Syntax (Base (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Util

type Parser = Parsec Void Text

parseVal :: FilePath -> Text -> Either Error (Val m)
parseVal fname s =
  case parse (spaceConsumer *> pVal <* eof) fname s of
    Left err -> Left $ T.pack $ errorBundlePretty err
    Right v -> Right v

pVal :: Parser (Val m)
pVal =
  choice
    [ arrayifyVal . ValBase <$> lBase,
      pArray,
      parens $ choice [pBox, pFlatArray]
    ]

pArray :: Parser (Val m)
pArray = brackets $ do
  vs <- many pVal
  case map valShapeOf vs of
    shape : shapes | any (/= shape) shapes -> fail "irregular array"
    _ -> pure $ collapse $ ValArray [length vs] vs

pFlatArray :: Parser (Val m)
pFlatArray = do
  symbol "array"
  shape <- brackets $ many lDecimal
  vs <- many pVal
  unless (product shape == length vs) $
    fail $
      "array of shape "
        <> prettyString shape
        <> " must have "
        <> prettyString (product shape)
        <> " elements, but has "
        <> prettyString (length vs)
  pure $ collapse $ ValArray shape vs

pBox :: Parser (Val m)
pBox = symbol "box" >> (ValBox <$> some pIndex <*> pVal)
  where
    pIndex = try $ lIndex <* notFollowedBy (symbol ")")

lBase :: Parser Base
lBase =
  choice
    [ BoolVal True <$ symbol "#t",
      BoolVal False <$ symbol "#f",
      try $ FloatVal <$> lexeme (L.signed empty L.float),
      IntVal <$> lexeme (L.signed empty L.decimal)
    ]

lIndex :: Parser (Either Int [Int])
lIndex =
  choice
    [ Right <$> brackets (lDecimal `sepBy` optional (symbol ",")),
      Left <$> lDecimal
    ]

lDecimal :: Parser Int
lDecimal = lexeme L.decimal

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment ";") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser ()
symbol = void . L.symbol spaceConsumer

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
