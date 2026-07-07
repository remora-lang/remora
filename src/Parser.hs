module Parser (parse, parseExp) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isDigit, isSpace)
import Data.Foldable (for_)
import Data.List.NonEmpty qualified as NE
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void
import Syntax hiding
  ( ArrayType,
    AtomType,
    Dim,
    ISpace,
    ISpaceParam,
    Shape,
    Type,
    TypeParam,
    TypeParamExp,
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
    manyTill,
    notFollowedBy,
    optional,
    satisfy,
    some,
    try,
    (<|>),
  )
import Text.Megaparsec qualified
import Text.Megaparsec.Char
  ( char,
    space1,
    string,
  )
import Text.Megaparsec.Char.Lexer qualified as L
import Util

type Parser = Parsec Void Text

type Dim = Syntax.Dim Text

type Shape = Syntax.Shape Text

type Type = Syntax.TypeExp Text

type TypeParamExp = Syntax.TypeParamExp Text

type ISpaceParam = Syntax.ISpaceParam Text

type ISpace = Syntax.ISpace Text

parse :: FilePath -> Text -> Either Error UncheckedProg
parse = parseWith pProg

parseExp :: FilePath -> Text -> Either Error UncheckedExp
parseExp = parseWith pExp

parseWith :: Parser a -> FilePath -> Text -> Either Error a
parseWith p fname s =
  case Text.Megaparsec.parse (spaceConsumer *> p <* eof) fname s of
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

keywords :: Set Text
keywords =
  S.fromList
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
      "Π",
      "Sigma",
      "Σ",
      "let",
      "type",
      "ispace",
      "fun",
      "t-fun",
      "i-fun",
      "val",
      "def",
      "entry"
    ]

lKeyword :: Text -> Parser ()
lKeyword s
  | s `S.member` keywords =
      lexeme $ void $ try (string s) <* notFollowedBy (satisfy isAlphaNum)
  | otherwise = fail $ "not a keyword: " <> T.unpack s

withSrcPos :: Parser (SourcePos -> a) -> Parser a
withSrcPos p = do
  pos <- getSourcePos
  ($ pos) <$> p

withNoInfo :: Parser (NoInfo b -> a) -> Parser a
withNoInfo = fmap ($ NoInfo)

withNoInfoPos :: Parser (NoInfo b -> SourcePos -> a) -> Parser a
withNoInfoPos = withSrcPos . withNoInfo

listOf :: Parser a -> Parser [a]
listOf = parens . many

someNE :: Parser a -> Parser (NE.NonEmpty a)
someNE p = (NE.:|) <$> p <*> many p

neListOf :: Parser a -> Parser (NE.NonEmpty a)
neListOf = parens . someNE

parseOptional :: Text -> Parser a -> Parser (Maybe a)
parseOptional t p = choice [Just <$> p, Nothing <$ symbol t]

lId :: Parser Text
lId = lexeme $ try $ do
  c <- satisfy (\c -> not (isDigit c) && not (isDisallowed c))
  cs <- many (satisfy $ not . isDisallowed)
  let x = T.pack $ c : cs
  if x `S.member` keywords
    then fail $ "unexpected keyword: " <> T.unpack x
    else pure x
  where
    isDisallowed c =
      isSpace c
        || c `elem` ['(', ')', '[', ']', '{', '}', '"', ',', '\'', '`', ';', '#', '|', '\\', '@']

pDecimal :: Parser Int
pDecimal = lexeme L.decimal

pTypeParamExp :: Parser TypeParamExp
pTypeParamExp =
  choice
    [ TEAtomTypeParam <$> ("&" >> lId),
      TEArrayTypeParam <$> ("*" >> lId)
    ]

pISpaceParam :: Parser ISpaceParam
pISpaceParam =
  choice
    [ DimParam <$> ("$" >> lId),
      ShapeParam <$> ("@" >> lId)
    ]

pType :: Parser Type
pType =
  withSrcPos $
    choice
      [ TEAtomVar <$> ("&" >> lId),
        TEArrayVar <$> ("*" >> lId),
        symbol "Bool" >> pure TEBool,
        symbol "Int" >> pure TEInt,
        symbol "Float" >> pure TEFloat,
        brackets $ TEArray <$> pType <*> pShapeSplice,
        parens $
          choice
            [ TEArray <$> (lKeyword "A" >> pType) <*> (ispaceToShape <$> pISpace),
              (lKeyword "->" <|> lKeyword "→") >> do
                args <- try (listOf pType) <|> ((: []) <$> pType)
                ret <- pType
                pure $ \pos -> foldr (\a r -> TEArrow a r pos) ret args,
              pBinder TEForall ["Forall", "∀"] pTypeParamExp,
              pBinder TEPi ["Pi", "Π"] pISpaceParam,
              pBinder TESigma ["Sigma", "Σ"] pISpaceParam
            ]
      ]
  where
    pBinder ::
      (NE.NonEmpty a -> Type -> SourcePos -> Type) ->
      [Text] ->
      Parser a ->
      Parser (SourcePos -> Type)
    pBinder c kws pParam = do
      choice $ map lKeyword kws
      params <- neListOf pParam
      body <- pType
      pure $ \pos -> c params body pos

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
          [ True <$ symbol "#t",
            False <$ symbol "#f"
          ]
    pNum =
      try $
        choice
          [ try $ FloatVal <$> L.signed (pure ()) (lexeme L.float),
            IntVal . fromIntegral <$> L.signed (pure ()) pDecimal
          ]

pAtom :: Parser UncheckedAtom
pAtom =
  choice
    [ withSrcPos $ withNoInfo $ Base <$> pBase,
      parens $ do
        pos <- getSourcePos
        choice
          [ pLambda ["fn", "λ"] Lambda pPat pos,
            pLambda ["i-fn", "iλ"] ILambda pISpaceParam pos,
            pLambda ["t-fn", "tλ"] TLambda pTypeParamExp pos,
            pBox pos
          ]
    ]
  where
    pLambda kws c pParam pos = do
      choice $ map lKeyword kws
      x NE.:| xs <- neListOf pParam
      body <- pExp
      pure $ c x (foldr (\p b -> mkScalar $ c p b NoInfo pos) body xs) NoInfo pos

    pBox pos = do
      ispaces <- lKeyword "box" >> neListOf pISpace
      e <- pExp
      nest ispaces e <$> pType
      where
        nest (d NE.:| []) e t = Box d e t NoInfo pos
        nest (d NE.:| (d' : ds)) e t =
          Box d (mkScalar $ nest (d' NE.:| ds) e (peelSigma t)) t NoInfo pos
        peelSigma (TESigma (_ NE.:| []) body _) = body
        peelSigma (TESigma (_ NE.:| (p' : ps)) body tpos) = TESigma (p' NE.:| ps) body tpos
        peelSigma t = t

pISpace :: Parser ISpace
pISpace =
  choice
    [ try $ Syntax.Dim <$> pDim,
      Syntax.Shape <$> pShape
    ]

pPat :: Parser UncheckedPat
pPat =
  withSrcPos $
    withNoInfo $
      parens $
        PatId <$> lId <*> pType

pExp :: Parser UncheckedExp
pExp =
  choice
    [ try $ withNoInfoPos $ Array mempty . pure <$> pAtom,
      brackets $ do
        es <- someNE pExp
        flattenExp <$> withSrcPos (withNoInfo $ pure $ Frame [NE.length es] es),
      choice
        [ withNoInfoPos $ Var <$> lId,
          withNoInfoPos pString,
          parens $
            choice $
              map
                withNoInfoPos
                [ do
                    shape <- lKeyword "array" >> pShapeLit
                    choice [try $ EmptyArray shape <$> pType, Array shape <$> someNE pAtom],
                  do
                    shape <- lKeyword "frame" >> pShapeLit
                    choice [try $ EmptyFrame shape <$> pType, Frame shape <$> someNE pExp],
                  pUnbox,
                  pLet,
                  pAtFn
                ]
                ++ [pApp, pIApp, pTApp]
        ]
    ]
  where
    pApp = pAnyApp App pExp Nothing
    pIApp = pAnyApp IApp pISpace $ Just "i-app"
    pTApp = pAnyApp TApp pType $ Just "t-app"
    pAnyApp c pArg kw = do
      pos <- getSourcePos
      for_ kw lKeyword
      f <- pExp
      args <- some pArg
      pure $ foldl' (\fun a -> c fun a NoInfo pos) f args
    pString = do
      s <- lexeme $ char '"' >> manyTill L.charLiteral (char '"')
      pos <- getSourcePos
      pure $ case s of
        [] -> EmptyArray [0] (TEInt pos)
        _ ->
          Array [length s] $
            NE.fromList $
              map (\c -> Base (IntVal $ fromEnum c) NoInfo pos) s
    pShapeLit = brackets $ many pDecimal
    pUnbox = do
      lKeyword "unbox"
      (eps, x, inner) <- parens $ (,,) <$> someNE pISpaceParam <*> lId <*> pExp
      outer <- pExp
      pure $ \_ pos -> nest eps x inner outer pos
      where
        nest (ep NE.:| []) x box body pos = Unbox ep x box body NoInfo pos
        nest (ep NE.:| (ep' : eps)) x box body pos =
          let inner = nest (ep' NE.:| eps) x (Var "#box" NoInfo pos) body pos
           in Unbox ep "#box" box inner NoInfo pos

    pLet =
      lKeyword "let"
        >> (Let <$> neListOf pBind <*> pExp)
      where

    pAtFn = do
      void $ symbol "@"
      f <- pExp
      mts <- parseOptional "_" $ listOf pType
      mispace <- parseOptional "_" $ listOf pISpace
      args <- many pExp
      let appBody pos = iApp pos mispace $ tApp pos mts f
      pure $
        \_ pos ->
          foldl' (\fun a -> App fun a NoInfo pos) (appBody pos) args
      where
        iApp _ Nothing m = m
        iApp pos (Just ispaces) m =
          foldl' (\fun a -> IApp fun a NoInfo pos) m ispaces

        tApp _ Nothing m = m
        tApp pos (Just ts) m =
          foldl' (\fun a -> TApp fun a NoInfo pos) m ts

pBind :: Parser UncheckedBind
pBind =
  withSrcPos $
    parens $
      choice
        [ uncurry BindVal
            <$> (lKeyword "val" >> pVarBind)
            <*> pExp,
          -- try because atFn also starts with fun
          try $ pFun BindFun "fun" (many pPat),
          pFun BindTFun "t-fun" (listOf pTypeParamExp),
          pFun BindIFun "i-fun" (listOf pISpaceParam),
          try pAtFnBind,
          lKeyword "type" >> (BindType <$> pTypeParamExp <*> pType <*> pure NoInfo),
          lKeyword "ispace" >> (BindISpace <$> pISpaceParam <*> pISpace)
        ]
  where
    pVarBind =
      choice
        [ (,) <$> lId <*> pure Nothing,
          parens $ (,) <$> (lId <* symbol ":") <*> (Just <$> pType)
        ]

    pFun c kw pParams =
      withNoInfo $ do
        lKeyword kw
        (f, params, t) <-
          parens $
            (,,) <$> lId <*> pParams <*> optional (symbol ":" *> pType)
        c f params t <$> pExp
    nestFunBind c f ps prevBind pos =
      let v = Var f NoInfo pos
       in c f ps Nothing (Let (pure (prevBind pos)) v NoInfo pos) NoInfo pos
    pAtFnBind = do
      lKeyword "fun"
      (f, mts, mispaces, params, t) <- parens $ do
        void $ symbol "@"
        f <- lId
        mts <- parseOptional "_" $ listOf pTypeParamExp
        mispaces <- parseOptional "_" $ listOf pISpaceParam
        params <- many pPat
        t <- optional $ symbol ":" *> pType
        pure (f, mts, mispaces, params, t)
      body <- pExp
      let funBind = BindFun f params t body NoInfo
          iBind =
            case mispaces of
              Nothing -> funBind
              Just ispaces ->
                nestFunBind BindIFun f ispaces funBind
          tBind =
            case mts of
              Nothing -> iBind
              Just ts ->
                nestFunBind BindTFun f ts iBind
      pure tBind

pDim :: Parser Dim
pDim =
  choice
    [ "$" >> DimVar <$> lId,
      DimN <$> pDecimal,
      parens $
        choice
          [ symbol "+" >> Add <$> many pDim,
            symbol "*" >> Mul <$> many pDim,
            symbol "-" >> Sub <$> many pDim
          ]
    ]

pShapeSplice :: Parser Shape
pShapeSplice =
  foldMap (mapISpace ShapeDim id) <$> many pISpace

pShape :: Parser Shape
pShape =
  choice
    [ "@" >> ShapeVar <$> lId,
      parens $
        choice
          [ lKeyword "dims" >> Syntax.Concat <$> many (ShapeDim <$> pDim),
            symbol "++" >> Concat <$> many pShape
          ],
      brackets pShapeSplice
    ]

pDecl :: Parser UncheckedDecl
pDecl =
  parens $
    choice
      [ lKeyword "def" >> Def <$> pBind,
        withNoInfoPos $ do
          lKeyword "entry"
          (f, params, t) <-
            parens $
              (,,) <$> lId <*> (many pPat) <*> optional (symbol ":" *> pType)
          Entry f params t <$> pExp
      ]

pProg :: Parser UncheckedProg
pProg = Syntax.Prog <$> many pDecl
