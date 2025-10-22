module Parser (parse) where

import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Syntax hiding (Atom, Dim, Exp, IVar, Shape, TVar, Type)
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
    option,
    satisfy,
    single,
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

type TVar = Syntax.TVar Text

type IVar = Syntax.IVar Text

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
      parens $ TArr <$> (lKeyword "A" >> pType) <*> pShape,
      parens $ (:->) <$> (lKeyword "->" >> parens (many pType)) <*> pType,
      parens $ Forall <$> (lKeyword "forall" >> parens (many pTVar)) <*> pType,
      parens $ Prod <$> (lKeyword "prod" >> parens (many pIVar)) <*> pType,
      parens $ Exists <$> (lKeyword "exists" >> parens (many pIVar)) <*> pType
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
      [ withUnchecked $ Base <$> pBase,
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
       in withUnchecked $
            Lambda
              <$> (lKeyword "fn" >> (parens $ many pArg))
              <*> pExp
    pILambda =
      withUnchecked $
        ILambda
          <$> (lKeyword "i-fn" >> (parens $ many pIVar))
          <*> pExp
    pTLambda =
      withUnchecked $
        TLambda
          <$> (lKeyword "t-fn" >> (parens $ many pTVar))
          <*> pExp
    pBox =
      Box <$> (many pShape) <*> pExp <*> pType

pExp :: Parser Exp
pExp =
  choice
    [ (withSrcPos $ withUnchecked $ (Array mempty . pure) <$> pAtom),
      between (symbol "[") (symbol "]") $ do
        es <- some pExp
        flattenExp <$> withSrcPos (withUnchecked $ pure $ Frame [length es] es),
      try pLet, -- fix
      withSrcPos
        ( withUnchecked
            ( choice
                [ Var <$> lId,
                  parens $
                    choice
                      [ Array <$> (lKeyword "array" >> pShapeLit) <*> some pAtom,
                        Frame <$> (lKeyword "frame" >> pShapeLit) <*> some pExp,
                        (. const Unchecked) <$> (App <$> pExp <*> some pExp),
                        lKeyword "i-app" >> IApp <$> pExp <*> some (Syntax.Shape <$> pShape),
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
        <$> (symbol "(" >> (many (pIVar <* notFollowedBy (symbol ")"))))
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
      params <- parens $ some pParam
      ret_t <- pType
      body <- pExp
      pos <- getSourcePos
      pure
        ( (f, map snd params :-> ret_t),
          Array mempty [Lambda params body Unchecked pos] Unchecked pos
        )

    pBind :: Parser ((Text, Type), Exp)
    pBind = parens $ pValBind <|> pFunBind
    bindToApp :: ((Text, Type), Exp) -> Exp -> Exp
    bindToApp ((param, e)) body =
      App
        ( Array
            mempty
            [Lambda [param] body Unchecked (posOf e)]
            Unchecked
            (posOf e)
        )
        [e]
        Unchecked
        (posOf e)

    pLet = parens $ do
      lKeyword "let"
      binds <- parens $ many pBind
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
