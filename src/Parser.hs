module Parser (parse) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Data.Proxy
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Syntax hiding (Atom, Exp, Idx, Type)
import Syntax qualified
import Text.Megaparsec
  ( Parsec,
    between,
    choice,
    empty,
    eof,
    errorBundlePretty,
    getSourcePos,
    many,
    notFollowedBy,
    optional,
    satisfy,
    some,
    try,
  )
import Text.Megaparsec qualified
import Text.Megaparsec.Char
  ( space1,
    string,
  )
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Pos
  ( SourcePos (..),
    mkPos,
  )

type Parser = Parsec Void Text

type Exp = Syntax.Exp Proxy Text

type Atom = Syntax.Atom Proxy Text

type Idx = Syntax.Idx Text

type Type = Syntax.Type Text

parse :: FilePath -> Text -> Either String Exp
parse fname s =
  case Text.Megaparsec.parse (spaceConsumer *> pExp <* eof) fname s of
    Left err -> Left $ errorBundlePretty err
    Right x -> Right x

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

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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

lKeyword :: Text -> Parser ()
lKeyword s
  | s `elem` keywords =
      lexeme $ void $ try (string s) <* notFollowedBy (satisfy isAlphaNum)
  | otherwise = fail $ "not a keyword: " <> T.unpack s

noSrcPos :: SourcePos
noSrcPos = SourcePos "<no location>" (mkPos 1) (mkPos 1)

withSrcPos :: Parser (SourcePos -> a) -> Parser a
withSrcPos p = do
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

pDecimal :: Parser Int
pDecimal = lexeme L.decimal

pType :: Parser Type
pType =
  choice
    [ TVar <$> lId,
      symbol "Bool" >> pure Bool,
      symbol "Int" >> pure Int,
      symbol "FLoat" >> pure Float
    ]

pSort :: Parser Sort
pSort =
  choice
    [ symbol "Shape" >> pure SortShape,
      symbol "Dim" >> pure SortDim
    ]

pAtom :: Parser Atom
pAtom =
  withSrcPos $
    choice
      [ pBool,
        pNum,
        pLambda,
        pILambda,
        pBox
      ]
  where
    pBool =
      BoolVal
        <$> choice
          [ symbol "#t" >> pure True,
            symbol "#f" >> pure False
          ]
    pNum =
      choice
        [ try $ IntVal <$> pDecimal,
          FloatVal <$> lexeme L.float
        ]
    pLambda =
      let pArg = parens $ (,) <$> lId <*> optional pType
       in parens $
            Lambda <$> (symbol "\\" >> (parens $ many pArg)) <*> pExp
    pILambda =
      let pArg = parens $ (,) <$> lId <*> optional pSort
       in parens $
            ILambda <$> (symbol "\\" >> (parens $ many pArg)) <*> pExp
    pBox =
      parens $
        Box <$> (many pIdx) <*> pExp <*> optional pType

pExp :: Parser Exp
pExp =
  withSrcPos $
    choice
      [ Var <$> lId,
        parens $
          Array <$> (lKeyword "array" >> pShapeLit) <*> some pAtom,
        parens $
          Frame <$> (lKeyword "frame" >> pShapeLit) <*> some pExp,
        parens $
          App <$> ((:) <$> pExp <*> some pExp),
        parens $
          IApp <$> pExp <*> (some pIdx),
        pUnbox
      ]
  where
    pShapeLit = parens $ many pDecimal
    pUnbox =
      parens $
        Unbox
          <$> (symbol "(" >> (many (lId <* notFollowedBy (symbol ")"))))
          <*> (pExp <* symbol ")")
          <*> pExp

pIdx :: Parser Idx
pIdx =
  withSrcPos $
    choice
      [ IdxVar <$> lId,
        Dim <$> pDecimal,
        Shape <$> many pIdx,
        parens $
          Add <$> many pIdx,
        parens $
          Concat <$> many pIdx
      ]
