module Parser where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Syntax hiding (Atom, Exp)
import Syntax qualified
import Text.Megaparsec
  ( Parsec,
    choice,
    empty,
    getSourcePos,
    many,
    satisfy,
    try,
  )
import Text.Megaparsec.Char
  ( space1,
  )
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos
  ( SourcePos (..),
    mkPos,
  )

type Parser = Parsec Void Text

type Exp = Syntax.Exp Proxy Text

type Atom = Syntax.Atom Proxy Text

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment ";")
    empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = L.symbol spaceConsumer

symbol_ :: Text -> Parser ()
symbol_ = void . symbol

keywords :: [Text]
keywords =
  [ "array",
    "frame",
    "t-app",
    "i-app",
    "unbox",
    "box",
    "shape"
  ]

noSrcPos :: SourcePos
noSrcPos = SourcePos "<no location>" (mkPos 1) (mkPos 1)

withSrcLoc :: Parser (SourcePos -> a) -> Parser a
withSrcLoc p = do
  pos <- getSourcePos
  ($ pos) <$> p

lId :: Parser Text
lId = lexeme $ try $ do
  x <- T.pack <$> many (satisfy $ not . isDisallowed)
  if x `elem` keywords
    then fail $ "unexpected keyword: " <> T.unpack x
    else pure x
  where
    isDisallowed c =
      isSpace c
        || c `elem` ['(', '[', ']', '{', '}', '"', ',', '\'', '`', ';', '#', '|', '\\']

pAtom :: Parser Atom
pAtom =
  choice
    [ pBool,
      pNum
    ]
  where
    pBool =
      withSrcLoc $
        BoolVal
          <$> choice
            [ symbol "#t" >> pure True,
              symbol "#f" >> pure False
            ]
    pNum =
      choice
        [ try $ withSrcLoc $ IntegerVal <$> L.decimal,
          withSrcLoc $ FloatVal <$> L.float
        ]
