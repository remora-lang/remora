module Parser (parse) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Syntax hiding (Atom, Dim, Exp, Shape, Type)
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
    satisfy,
    some,
    try,
    (<|>),
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

type Exp = Syntax.Exp Unchecked Text

type Atom = Syntax.Atom Unchecked Text

type Dim = Syntax.Dim Text

type Shape = Syntax.Shape Text

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

withUnchecked :: Parser (Unchecked (Syntax.Type Text) -> a) -> Parser a
withUnchecked = fmap ($ Unchecked)

lId :: Parser Text
lId = lexeme $ try $ do
  c <- satisfy (\c -> not (isDigit c) && not (isDisallowed c))
  cs <- many (satisfy $ not . isDisallowed)
  let x = T.pack $ c : cs
  if x `elem` keywords
    then fail $ "unexpected keyword: " <> T.unpack x
    else pure x
  where
    isDisallowed c =
      isSpace c
        || c `elem` ['(', ')', '[', ']', '{', '}', '"', ',', '\'', '`', ';', '#', '|', '\\']

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

pBase :: Parser Base
pBase =
  choice
    [ pBool,
      pNum
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

pAtom :: Parser Atom
pAtom =
  withSrcPos $
    choice
      [ withUnchecked $ Base <$> pBase,
        try $ -- fix
          parens $
            choice
              [ pLambda,
                pILambda,
                pBox
              ]
      ]
  where
    pLambda =
      let pArg = parens $ (,) <$> lId <*> pType
       in withUnchecked $
            Lambda
              <$> (symbol "\\" >> (parens $ many pArg))
              <*> pExp
    pILambda =
      let pArg = parens $ (,) <$> lId <*> pSort
       in withUnchecked $
            ILambda
              <$> (symbol "I\\" >> (parens $ many pArg))
              <*> pExp
    pBox =
      Box <$> (many pShape) <*> pExp <*> pType

pExp :: Parser Exp
pExp =
  withSrcPos
    ( withUnchecked
        ( choice
            [ Var <$> lId,
              parens $
                choice
                  [ Array <$> (lKeyword "array" >> pShapeLit) <*> some pAtom,
                    Frame <$> (lKeyword "frame" >> pShapeLit) <*> some pExp,
                    App <$> ((:) <$> pExp <*> some pExp),
                    IApp <$> pExp <*> (some pShape),
                    lKeyword "t-app" >> TApp <$> pExp <*> (some pType),
                    pUnbox
                  ]
            ]
        )
    )
    <|> (Syntax.Atom <$> pAtom)
  where
    pShapeLit = parens $ many pDecimal
    pUnbox =
      Unbox
        <$> (symbol "(" >> (many (lId <* notFollowedBy (symbol ")"))))
        <*> (pExp <* symbol ")")
        <*> pExp

pDim :: Parser Dim
pDim =
  choice
    [ DimVar <$> lId,
      Syntax.Dim <$> pDecimal
    ]

pShape :: Parser Shape
pShape =
  choice
    [ ShapeVar <$> lId,
      Syntax.Shape <$> many pDim,
      parens $
        Add <$> many pDim,
      parens $
        Concat <$> many pShape
    ]
