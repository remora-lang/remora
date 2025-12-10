module Parser (parse) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Syntax hiding
  ( ArrayType,
    Atom,
    AtomType,
    Bind,
    Dim,
    Exp,
    Extent,
    ExtentParam,
    Pat,
    Shape,
    Type,
    TypeParam,
  )
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

type Parser = Parsec Void Text

type Exp = Syntax.Exp NoInfo Text

type Atom = Syntax.Atom NoInfo Text

type Dim = Syntax.Dim Text

type Shape = Syntax.Shape Text

type Type = Syntax.TypeExp Text

type TypeParam = Syntax.TypeParam Text

type ExtentParam = Syntax.ExtentParam Text

type Extent = Syntax.Extent Text

type Pat = Syntax.Pat NoInfo Text

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

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

keywords :: [Text]
keywords =
  [ "array",
    "frame",
    "t-app",
    "i-app",
    "unbox",
    "box",
    "dims",
    "fn",
    "λ",
    "t-fn",
    "tλ",
    "i-fn",
    "iλ",
    "A",
    "->",
    "→",
    "Forall",
    "∀",
    "Pi",
    "П",
    "Sigma",
    "Σ",
    "let",
    "type",
    "extent",
    "fun",
    "t-fun",
    "i-fun",
    "val"
  ]

lKeyword :: Text -> Parser ()
lKeyword s
  | s `elem` keywords =
      lexeme $ void $ try (string s) <* notFollowedBy (satisfy isAlphaNum)
  | otherwise = fail $ "not a keyword: " <> T.unpack s

withSrcPos :: Parser (SourcePos -> a) -> Parser a
withSrcPos p = do
  pos <- getSourcePos
  ($ pos) <$> p

withNoInfo :: Parser (NoInfo b -> a) -> Parser a
withNoInfo = fmap ($ NoInfo)

listOf :: Parser a -> Parser [a]
listOf = parens . many

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

pTypeParam :: Parser TypeParam
pTypeParam =
  choice
    [ AtomTypeParam <$> ("&" >> lId),
      ArrayTypeParam <$> ("*" >> lId)
    ]

pExtentParam :: Parser ExtentParam
pExtentParam =
  choice
    [ DimParam <$> ("$" >> lId),
      ShapeParam <$> ("@" >> lId)
    ]

pType :: Parser Type
pType =
  withSrcPos $
    choice $
      [ TEAtomVar <$> ("&" >> lId),
        TEArrayVar <$> ("*" >> lId),
        symbol "Bool" >> pure TEBool,
        symbol "Int" >> pure TEInt,
        symbol "Float" >> pure TEFloat,
        brackets $ TEArray <$> pType <*> pShapeSplice,
        parens $
          choice
            [ TEArray <$> (lKeyword "A" >> pType) <*> pShape,
              TEArrow
                <$> ( (lKeyword "->" <|> lKeyword "→")
                        >> listOf pType
                    )
                <*> pType,
              TEForall
                <$> ( (lKeyword "Forall" <|> lKeyword "∀")
                        >> listOf pTypeParam
                    )
                <*> pType,
              TEPi
                <$> ( (lKeyword "Pi" <|> lKeyword "П")
                        >> listOf pExtentParam
                    )
                <*> pType,
              TESigma
                <$> ( (lKeyword "Sigma" <|> lKeyword "Σ")
                        >> listOf pExtentParam
                    )
                <*> pType
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
    withNoInfo $
      choice
        [ Base <$> pBase,
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
      Lambda
        <$> ((lKeyword "fn" <|> lKeyword "λ") >> (listOf pPat))
        <*> pExp
    pILambda =
      ILambda
        <$> ((lKeyword "i-fn" <|> lKeyword "iλ") >> (listOf pExtentParam))
        <*> pExp
    pTLambda =
      TLambda
        <$> ((lKeyword "t-fn" <|> lKeyword "tλ") >> (listOf pTypeParam))
        <*> pExp
    pBox =
      Box <$> (lKeyword "box" >> listOf pExtent) <*> pExp <*> pType

pExtent :: Parser Extent
pExtent =
  choice
    [ try $ Syntax.Dim <$> pDim,
      Syntax.Shape <$> pShape
    ]

pPat :: Parser Pat
pPat =
  withSrcPos $
    withNoInfo $
      parens $
        PatId <$> lId <*> pType

pExp :: Parser Exp
pExp =
  choice
    [ (withSrcPos $ withNoInfo $ (Array mempty . pure) <$> pAtom),
      between (symbol "[") (symbol "]") $ do
        es <- some pExp
        flattenExp <$> withSrcPos (withNoInfo $ pure $ Frame [length es] es),
      withSrcPos
        ( withNoInfo
            ( choice
                [ Var <$> lId,
                  parens $
                    choice
                      [ Array <$> (lKeyword "array" >> pShapeLit) <*> some pAtom,
                        Frame <$> (lKeyword "frame" >> pShapeLit) <*> some pExp,
                        (. const NoInfo) <$> (App <$> pExp <*> many pExp),
                        lKeyword "i-app" >> IApp <$> pExp <*> many pExtent,
                        lKeyword "t-app" >> TApp <$> pExp <*> (many pType),
                        pUnbox,
                        pLet,
                        pAtFn
                      ]
                ]
            )
        )
    ]
  where
    pShapeLit = brackets $ many pDecimal
    pUnbox =
      Unbox
        <$> ( lKeyword "unbox"
                >> (symbol "(" >> (many (pExtentParam <* notFollowedBy (symbol ")"))))
            )
        <*> lId
        <*> (pExp <* symbol ")")
        <*> pExp

    pLet =
      lKeyword "let"
        >> (Let <$> listOf pBind <*> pExp)
      where
        pBind =
          withSrcPos $
            parens $
              choice
                [ try $
                    BindVal
                      <$> (lKeyword "val" >> lId)
                      <*> pure Nothing
                      <*> pExp,
                  try $
                    BindVal
                      <$> (lKeyword "val" >> symbol "(" *> lId <* symbol ":")
                      <*> ((Just <$> pType) <* symbol ")")
                      <*> pExp,
                  try $
                    withNoInfo $
                      BindFun
                        <$> (lKeyword "fun" *> symbol "(" *> lId)
                        <*> many pPat
                        <*> (((symbol ":" *> (Just <$> pType)) <|> pure Nothing) <* symbol ")")
                        <*> pExp,
                  try $ pAtFnBind,
                  withNoInfo $
                    BindTFun
                      <$> (lKeyword "t-fun" *> symbol "(" *> lId)
                      <*> listOf pTypeParam
                      <*> (((symbol ":" *> (Just <$> pType)) <|> pure Nothing) <* symbol ")")
                      <*> pExp,
                  lKeyword "i-fun"
                    >> ( withNoInfo $
                           BindIFun
                             <$> (symbol "(" *> lId)
                             <*> listOf pExtentParam
                             <*> (((symbol ":" *> (Just <$> pType)) <|> pure Nothing) <* symbol ")")
                             <*> pExp
                       ),
                  lKeyword "type" >> (BindType <$> pTypeParam <*> pType <*> pure NoInfo),
                  lKeyword "extent" >> (BindExtent <$> pExtentParam <*> pExtent)
                ]

    pAtFn = do
      void $ symbol "@"
      f <- pExp
      mts <- choice [Just <$> listOf pType, symbol "_" >> pure Nothing]
      mextent <- choice [Just <$> listOf pExtent, symbol "_" >> pure Nothing]
      args <- many pExp
      pure $
        \_ pos ->
          App (iApp pos mextent $ tApp pos mts f) args NoInfo pos
      where
        iApp _ Nothing m = m
        iApp pos (Just extents) m =
          IApp m extents NoInfo pos

        tApp _ Nothing m = m
        tApp pos (Just ts) m =
          TApp m ts NoInfo pos

    -- TODO: fix this atrocity
    pAtFnBind =
      choice
        [ -- do
          --   void $ symbol "@"
          --   f <- lId
          --   mts <- choice [Just <$> listOf pTypeParam, symbol "_" >> pure Nothing]
          --   mextents <- choice [Just <$> listOf pExtentParam, symbol "_" >> pure Nothing]
          --   params <- listOf pParam
          --   body <- pExp
          --   pure $ \pos ->
          --     BindFun f params Nothing (tBind pos mts $ iBind pos mextents body) pos,
          do
            lKeyword "fun"
            symbol "("
            void $ symbol "@"
            f <- lId
            mts <- choice [Just <$> listOf pTypeParam, symbol "_" >> pure Nothing]
            mextents <- choice [Just <$> listOf pExtentParam, symbol "_" >> pure Nothing]
            params <- many pPat
            symbol ":"
            t <- pType
            symbol ")"
            body <- pExp
            let funBind pos = BindFun f params (Just t) body NoInfo pos
                iBind pos =
                  case mextents of
                    Nothing -> funBind pos
                    Just extents ->
                      BindIFun
                        f
                        extents
                        Nothing
                        (Let [funBind pos] (Var f NoInfo pos) NoInfo pos)
                        NoInfo
                        pos

                tBind pos =
                  case mts of
                    Nothing -> iBind pos
                    Just ts ->
                      BindTFun
                        f
                        ts
                        Nothing
                        (Let [iBind pos] (Var f NoInfo pos) NoInfo pos)
                        NoInfo
                        pos
            pure tBind
        ]

pDim :: Parser Dim
pDim =
  choice
    [ "$" >> DimVar <$> lId,
      DimN <$> pDecimal,
      parens $ (symbol "+" >> Add <$> many pDim) <|> (symbol "*" >> Mul <$> many pDim)
    ]

pShapeSplice :: Parser Shape
pShapeSplice =
  (foldMap $ mapExtent ShapeDim id) <$> many pExtent

pShape :: Parser Shape
pShape =
  choice
    [ "@" >> ShapeVar <$> lId,
      try $ Syntax.ShapeDim <$> pDim,
      parens $
        choice
          [ lKeyword "dims" >> Syntax.Concat <$> many (ShapeDim <$> pDim),
            symbol "++" >> Concat <$> many pShape
          ],
      brackets $ pShapeSplice
    ]
