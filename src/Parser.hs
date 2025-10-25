module Parser (parse) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Syntax hiding (Atom, Dim, Exp, IVar, Idx, Shape, TVar, Type)
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

type Exp = Syntax.Exp NoInfo Text

type Atom = Syntax.Atom NoInfo Text

type Dim = Syntax.Dim Text

type Shape = Syntax.Shape Text

type Type = Syntax.Type Text

type TVar = Syntax.TVar Text

type IVar = Syntax.IVar Text

type Idx = Syntax.Idx Text

parse :: FilePath -> Text -> Either Text Exp
parse fname s =
  case Text.Megaparsec.parse (spaceConsumer *> pExp <* eof) fname s of
    Left err -> Left $ T.pack $ errorBundlePretty err
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
    "shape",
    "fn",
    "t-fn",
    "i-fn",
    "A",
    "->",
    "forall",
    "prod",
    "exists",
    "let"
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

withNoInfo :: Parser (NoInfo (Syntax.Type Text) -> a) -> Parser a
withNoInfo = fmap ($ NoInfo)

manyLisp :: Parser a -> Parser [a]
manyLisp p =
  choice
    [ pure <$> try p,
      parens $ many p
    ]

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
        || c `elem` ['(', ')', '[', ']', '{', '}', '"', ',', '\'', '`', ';', '#', '|', '\\', '@']

pDecimal :: Parser Int
pDecimal = lexeme L.decimal

pTVar :: Parser TVar
pTVar =
  choice
    [ AtomTVar <$> ("@" >> lId),
      ArrayTVar <$> ("*" >> lId)
    ]

pIVar :: Parser IVar
pIVar =
  choice
    [ DVar <$> ("$" >> lId),
      SVar <$> ("@" >> lId)
    ]

pType :: Parser Type
pType =
  choice
    [ Syntax.TVar <$> pTVar,
      symbol "Bool" >> pure Bool,
      symbol "Int" >> pure Int,
      symbol "Float" >> pure Float,
      parens $
        choice
          [ TArr <$> (lKeyword "A" >> pType) <*> pShape,
            (:->) <$> (lKeyword "->" >> manyLisp pType) <*> pType,
            Forall <$> (lKeyword "forall" >> manyLisp pTVar) <*> pType,
            Prod <$> (lKeyword "prod" >> manyLisp pIVar) <*> pType,
            Exists <$> (lKeyword "exists" >> manyLisp pIVar) <*> pType
          ]
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
        [ try $ FloatVal <$> lexeme L.float,
          IntVal <$> pDecimal
        ]

pAtom :: Parser Atom
pAtom =
  withSrcPos $
    choice
      [ withNoInfo $ Base <$> pBase,
        try $
          parens $
            choice
              [ pLambda,
                pTLambda,
                pILambda,
                pBox
              ]
      ]
  where
    pLambda =
      let pArg = parens $ (,) <$> lId <*> pType
       in withNoInfo $
            Lambda
              <$> (lKeyword "fn" >> (manyLisp pArg))
              <*> pExp
    pILambda =
      withNoInfo $
        ILambda
          <$> (lKeyword "i-fn" >> (manyLisp pIVar))
          <*> pExp
    pTLambda =
      withNoInfo $
        TLambda
          <$> (lKeyword "t-fn" >> (manyLisp pTVar))
          <*> pExp
    pBox =
      Box <$> (lKeyword "box" >> many pIdx) <*> pExp <*> pType

pIdx :: Parser Idx
pIdx =
  choice
    [ Syntax.Dim <$> pDim,
      Syntax.Shape <$> pShape
    ]

pExp :: Parser Exp
pExp =
  choice
    [ (withSrcPos $ withNoInfo $ (Array mempty . pure) <$> pAtom),
      between (symbol "[") (symbol "]") $ do
        es <- some pExp
        flattenExp <$> withSrcPos (withNoInfo $ pure $ Frame [length es] es),
      try pLet, -- fix
      withSrcPos
        ( withNoInfo
            ( choice
                [ Var <$> lId,
                  parens $
                    choice
                      [ Array <$> (lKeyword "array" >> pShapeLit) <*> some pAtom,
                        Frame <$> (lKeyword "frame" >> pShapeLit) <*> some pExp,
                        (. const NoInfo) <$> (App <$> pExp <*> some pExp),
                        lKeyword "i-app" >> IApp <$> pExp <*> some pIdx,
                        lKeyword "t-app" >> TApp <$> pExp <*> (some pType),
                        pUnbox
                      ]
                ]
            )
        )
    ]
  where
    pShapeLit = parens $ many pDecimal
    pUnbox =
      Unbox
        <$> (lKeyword "unbox" >> (symbol "(" >> (many (pIVar <* notFollowedBy (symbol ")")))))
        <*> lId
        <*> (pExp <* symbol ")")
        <*> pExp

    pParam :: Parser (Text, Type)
    pParam = parens $ (,) <$> lId <*> pType

    pValBind :: Parser ((Text, Type), Exp)
    pValBind =
      (,) <$> pParam <*> pExp

    pFunBind :: Parser ((Text, Type), Exp)
    pFunBind = do
      f <- lId
      params <- manyLisp pParam
      ret_t <- pType
      body <- pExp
      pos <- getSourcePos
      pure
        ( (f, map snd params :-> ret_t),
          Array mempty [Lambda params body NoInfo pos] NoInfo pos
        )

    pBind :: Parser ((Text, Type), Exp)
    pBind = parens $ pValBind <|> pFunBind
    bindToApp :: ((Text, Type), Exp) -> Exp -> Exp
    bindToApp ((param, e)) body =
      App
        ( Array
            mempty
            [Lambda [param] body NoInfo (posOf e)]
            NoInfo
            (posOf e)
        )
        [e]
        NoInfo
        (posOf e)

    pLet = parens $ do
      lKeyword "let"
      binds <- manyLisp pBind
      body <- pExp
      pure $ foldr bindToApp body binds

pDim :: Parser Dim
pDim =
  choice
    [ "$" >> DimVar <$> lId,
      DimN <$> pDecimal,
      symbol "+" >> Add <$> many pDim
    ]

pShape :: Parser Shape
pShape =
  choice
    [ "@" >> ShapeVar <$> lId,
      Syntax.ShapeDim <$> pDim,
      parens $
        choice
          [ lKeyword "shape" >> Syntax.Concat <$> many (ShapeDim <$> pDim),
            symbol "++" >> Concat <$> many pShape
          ]
    ]
