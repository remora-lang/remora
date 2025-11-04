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

type Type = Syntax.Type NoInfo Text

type AtomType = Syntax.AtomType NoInfo Text

type ArrayType = Syntax.ArrayType NoInfo Text

type TypeParam = Syntax.TypeParam Text

type ExtentParam = Syntax.ExtentParam Text

type Extent = Syntax.Extent Text

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

pAtomType :: Parser AtomType
pAtomType =
  choice
    [ AtomTypeVar <$> ("&" >> lId),
      symbol "Bool" >> pure Bool,
      symbol "Int" >> pure Int,
      symbol "Float" >> pure Float,
      parens $
        choice
          [ (:->) <$> ((lKeyword "->" <|> lKeyword "→") >> listOf pArrayType) <*> pArrayType,
            Forall <$> ((lKeyword "Forall" <|> lKeyword "∀") >> listOf pTypeParam) <*> pArrayType,
            Pi <$> ((lKeyword "Pi" <|> lKeyword "П") >> listOf pExtentParam) <*> pArrayType,
            Sigma <$> ((lKeyword "Sigma" <|> lKeyword "Σ") >> listOf pExtentParam) <*> pArrayType
          ]
    ]

pArrayType' :: Parser ArrayType
pArrayType' =
  choice
    [ try $ parens $ A <$> (lKeyword "A" >> pAtomType) <*> pShape,
      ArrayTypeVar <$> ("*" >> lId) <*> pure NoInfo <*> pure NoInfo,
      brackets $ A <$> pAtomType <*> pShapeSplice
    ]

pArrayType :: Parser ArrayType
pArrayType =
  choice
    [ pArrayType',
      (flip A mempty) <$> pAtomType
    ]

pType :: Parser Type
pType =
  choice
    [ Syntax.ArrayType <$> pArrayType',
      Syntax.AtomType <$> pAtomType
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
      let pArg = parens $ (,) <$> lId <*> pArrayType
       in withNoInfo $
            Lambda
              <$> ((lKeyword "fn" <|> lKeyword "λ") >> (listOf pArg))
              <*> pExp
    pILambda =
      withNoInfo $
        ILambda
          <$> ((lKeyword "i-fn" <|> lKeyword "iλ") >> (listOf pExtentParam))
          <*> pExp
    pTLambda =
      withNoInfo $
        TLambda
          <$> ((lKeyword "t-fn" <|> lKeyword "tλ") >> (listOf pTypeParam))
          <*> pExp
    pBox =
      Box <$> (lKeyword "box" >> listOf pExtent) <*> pExp <*> pAtomType

pExtent :: Parser Extent
pExtent =
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

    pParam :: Parser (Text, ArrayType)
    pParam = parens $ (,) <$> lId <*> pArrayType

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
                      <$> (symbol "(" *> lId <* symbol ":")
                      <*> ((Just <$> pArrayType) <* symbol ")")
                      <*> pExp,
                  try $
                    BindFun
                      <$> (lKeyword "fun" *> symbol "(" *> lId)
                      <*> many pParam
                      <*> (((symbol ":" *> (Just <$> pArrayType)) <|> pure Nothing) <* symbol ")")
                      <*> pExp,
                  pAtFnBind,
                  lKeyword "t-fun"
                    >> ( BindTFun
                           <$> lId
                           <*> listOf pTypeParam
                           <*> (option Nothing (Just <$> pArrayType))
                           <*> pExp
                       ),
                  lKeyword "i-fun"
                    >> ( BindIFun
                           <$> lId
                           <*> listOf pExtentParam
                           <*> (option Nothing (Just <$> pArrayType))
                           <*> pExp
                       ),
                  lKeyword "type" >> (BindType <$> pTypeParam <*> pType),
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
            params <- many pParam
            symbol ":"
            t <- pArrayType
            symbol ")"
            let fun_type = mkScalarArrayType $ map snd params :-> t
            body <- pExp
            let funBind pos = BindFun f params (Just t) body pos
                iBind pos =
                  case mextents of
                    Nothing -> funBind pos
                    Just extents ->
                      BindIFun
                        f
                        extents
                        (Just fun_type)
                        (Let [funBind pos] (Var f NoInfo pos) NoInfo pos)
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
                        pos
            pure tBind
        ]

pDim :: Parser Dim
pDim =
  choice
    [ "$" >> DimVar <$> lId,
      DimN <$> pDecimal,
      parens $ symbol "+" >> Add <$> many pDim,
      parens $ symbol "*" >> Mul <$> many pDim
    ]

pShapeSplice :: Parser Shape
pShapeSplice =
  (foldMap $ mapExtent ShapeDim id) <$> many pExtent

pShape :: Parser Shape
pShape =
  choice
    [ "@" >> ShapeVar <$> lId,
      Syntax.ShapeDim <$> pDim,
      parens $
        choice
          [ lKeyword "dims" >> Syntax.Concat <$> many (ShapeDim <$> pDim),
            symbol "++" >> Concat <$> many pShape
          ],
      brackets $ pShapeSplice
    ]
