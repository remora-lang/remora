module Futhark (compile, compileExp) where

import Control.Monad
import Control.Monad.Error.Class
import Data.List (group, singleton, sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe
import Data.Text qualified as T
import Futhark.Construct qualified as F
import Futhark.IR.SOACS qualified as F
import Futhark.Intrinsics (compileIntrinsic)
import Futhark.Monad
import Pass
import Prettyprinter hiding (group)
import Prettyprinter.Render.Text
import Prop
import Syntax hiding (ArrayType)
import Util
import VName

compileParam :: Pat -> FutharkM (F.LParam F.SOACS)
compileParam (PatId v _ (Info t) _) = do
  let v' = compileVName v
  t' <- compileArrayType t
  pure $ F.Param mempty v' t'

compileAtom :: Atom -> FutharkM F.SubExp
compileAtom (Base (BoolVal x) _ _) =
  pure $ F.Constant $ F.BoolValue x
compileAtom (Base (IntVal x) _ _) =
  pure $ F.Constant $ F.IntValue $ F.Int64Value $ fromIntegral x
compileAtom (Base (FloatVal x) _ _) =
  pure $ F.Constant $ F.FloatValue $ F.Float32Value x
compileAtom e = error $ unlines ["compileAtom: unhandled:", prettyString e]

compileFunName :: VName -> F.Name
compileFunName = F.nameFromText . prettyText

retAls :: Int -> F.RetAls
retAls n = F.RetAls [0 .. n - 1] [0]

applyName :: VName -> [Arg] -> ArrayType -> FutharkM F.SubExp
applyName v = fromMaybe (compileApp $ compileFunName v) (compileIntrinsic v)

compileExp' :: Exp -> FutharkM F.SubExp
compileExp' e@(Array ds atoms _ _) = do
  elems <- NE.toList <$> mapM compileAtom atoms
  nestArrayLits ds elems $ arrayTypeOf e
compileExp' (EmptyArray _ _ (Info t) _) =
  F.letSubExp "empty" =<< F.eBlank =<< compileArrayType t
compileExp' (EmptyFrame _ _ (Info t) _) =
  F.letSubExp "empty" =<< F.eBlank =<< compileArrayType t
compileExp' e@(Frame ds es _ _) = do
  elems <- NE.toList <$> mapM compileExp' es
  nestArrayLits ds elems $ arrayTypeOf e
compileExp' e@(Var v (Info t) _)
  | Just intrinsic <- compileIntrinsic v =
      if isFunctionType t
        then error $ unlines ["compileExp': unapplied intrinsic:", prettyString e]
        else intrinsic [] t
compileExp' (Var v _ _) =
  pure $ F.Var $ compileVName v
compileExp' e@(App _ _ (Info (t, pframe)) _)
  | (f@(Var f' _ _), allArgs) <- unfoldApp e,
    isScalar (typeOf f) = do
      let (paramTys, _) = unfoldArrow (arrayTypeOf f)
      unless (length paramTys == length allArgs) $
        error $
          unlines ["compileExp': partial application:", prettyString e]
      args <- zipWithM mkArg paramTys allArgs
      withMapNest (intShape pframe) t args $ applyName f'
  | otherwise =
      error $
        unlines
          [ "compileExp': unhandled lifted apply with func array:",
            prettyString e
          ]
compileExp' (Let bs e _ _) =
  mapM_ compileBind bs >> compileExp' e
compileExp' e = error $ unlines ["compileExp': unhandled:", prettyString e]

nestArrayLits :: [Int] -> [F.SubExp] -> ArrayType -> FutharkM F.SubExp
nestArrayLits [] [e] _ = pure e
nestArrayLits (_ : ds) elems t = do
  let inner_t = peelArrayType t
  elems' <- mapM (\es -> nestArrayLits ds es inner_t) $ chunksOf (product ds) elems
  inner_t' <- compileArrayType inner_t
  F.letSubExp "array" $ F.BasicOp $ F.ArrayLit elems' inner_t'
nestArrayLits ds elems t =
  error $
    "nestArrayLits: "
      ++ prettyString ds
      ++ " does not describe "
      ++ show (length elems)
      ++ " elements of "
      ++ prettyString t

mkArg :: ArrayType -> Exp -> FutharkM Arg
mkArg t x | isFunctionType t = FunArg <$> compileFunArg x
  where
    compileFunArg e@(Var f (Info fty) _) =
      case unfoldArrow fty of
        ([], _) ->
          error $ unlines ["compileFunArg: not a function type:", prettyString e]
        (paramTys, retTy) -> do
          params <- forM paramTys $ \t' -> F.newParam "x" =<< compileArrayType t'
          let args =
                zipWith (\p -> Arg . SExpArg [] (F.Var $ F.paramName p)) params paramTys
          F.mkLambda params $
            singleton . F.subExpRes <$> applyName f args retTy
    compileFunArg e =
      error $ unlines ["compileFunArg: unhandled:", prettyString e]
mkArg t_param x = do
  x' <- compileExp' x
  pure $
    Arg
      SExpArg
        { argFrame =
            let argShape = intShape $ shapeOf x
                paramShape = intShape $ arrayTypeShape t_param
             in take (length argShape - length paramShape) argShape,
          argSExp = x',
          argType = arrayTypeOf x
        }

compileApp :: F.Name -> [Arg] -> ArrayType -> FutharkM F.SubExp
compileApp f args t = do
  t' <- compileArrayType t
  F.letSubExp "apply" $
    F.Apply
      f
      (map ((,F.Observe) . argSExp . sexpArg) args)
      [(F.staticShapes1 $ F.toDecl t' F.Nonunique, retAls $ length args)]
      F.Safe
  where
    sexpArg (Arg a) = a
    sexpArg (FunArg _) =
      error $ "compileApp: function argument to " ++ F.nameToString f

withMapNest ::
  [Int] ->
  ArrayType ->
  [Arg] ->
  ([Arg] -> ArrayType -> FutharkM F.SubExp) ->
  FutharkM F.SubExp
withMapNest [] t args m = m args t
withMapNest (d : ds) t args m = do
  argPairs <- mapM mapArg args
  let (inputs, params) = unzip $ mapMaybe fst argPairs
      args' = map snd argPairs
  lam <-
    F.mkLambda params $
      singleton . F.subExpRes <$> withMapNest ds (peelArrayType t) args' m
  form <- F.mapSOAC lam
  F.letSubExp "map" $ F.Op $ F.Screma (constInt64 d) inputs form
  where
    mapArg :: Arg -> FutharkM (Maybe (F.VName, F.LParam F.SOACS), Arg)
    mapArg (Arg (SExpArg (_ : frame) se aty)) = do
      input <- F.letExp "xs" $ F.BasicOp $ F.SubExp se
      param <- F.newParam "x" =<< compileArrayType (peelArrayType aty)
      pure
        ( Just (input, param),
          Arg $ SExpArg frame (F.Var $ F.paramName param) $ peelArrayType aty
        )
    mapArg arg = pure (Nothing, arg)

compileBind :: Bind -> FutharkM ()
compileBind BindType {} = pure ()
compileBind BindISpace {} = pure ()
compileBind (BindFun f params _ body (Info ret) _) = do
  (params', ret') <-
    assertNoStms $
      (,)
        <$> mapM compileParam (NE.toList params)
        <*> compileArrayType (findRet ret)
  body' <-
    F.localScope (F.scopeOfLParams params') $
      mkBody $
        pure <$> compileExp' body
  addFunction
    F.FunDef
      { F.funDefEntryPoint = Nothing,
        F.funDefAttrs = mempty,
        F.funDefName = compileFunName f,
        F.funDefRetType =
          [ ( F.toDecl (F.staticShapes1 ret') F.Nonunique,
              retAls $ length params'
            )
          ],
        F.funDefParams = map (fmap (`F.toDecl` F.Nonunique)) params',
        F.funDefBody = body'
      }
compileBind (BindVal v _ e _) = do
  e' <- F.BasicOp . F.SubExp <$> compileExp' e
  F.letBindNames [compileVName v] e'
compileBind b = error $ "compileBind: unhandled " ++ prettyString b

valueType :: F.Type -> F.ValueType
valueType (F.Prim pt) =
  F.ValueType F.Signed mempty pt
valueType (F.Array pt shape _) =
  F.ValueType F.Signed (F.Rank (F.shapeRank shape)) pt
valueType t = error $ "valueType: unhandled " ++ F.prettyString t

addEntry :: F.Name -> [F.LParam F.SOACS] -> Exp -> FutharkM ()
addEntry name params body = do
  (res, stms) <-
    F.collectStms $
      F.localScope (F.scopeOfLParams params) $
        compileExp' body
  ret <- assertNoStms $ compileArrayType $ arrayTypeOf body
  let entryResult =
        F.EntryResult
          { F.entryResultUniqueness = F.Nonunique,
            F.entryResultType = F.TypeTransparent $ valueType ret
          }
  addFunction
    F.FunDef
      { F.funDefEntryPoint = Just (name, map entryParam params, entryResult, Nothing),
        F.funDefAttrs = mempty,
        F.funDefName = "entry_" <> name,
        F.funDefRetType =
          [(F.toDecl (F.staticShapes1 ret) F.Nonunique, retAls $ length params)],
        F.funDefParams = map (fmap (`F.toDecl` F.Nonunique)) params,
        F.funDefBody = F.Body () stms [F.SubExpRes mempty res]
      }
  where
    entryParam p =
      F.EntryParam
        { F.entryParamName = F.baseName $ F.paramName p,
          F.entryParamUniqueness = F.Nonunique,
          F.entryParamType = F.TypeTransparent $ valueType $ F.paramType p
        }

compileDecl :: Decl -> FutharkM ()
compileDecl (Def b) = compileBind b
compileDecl (Entry f params _ body _ _) = do
  params' <- assertNoStms $ mapM compileParam params
  addEntry (F.nameFromText $ varName f) params' body

runCompile :: FutharkM () -> PassM T.Text
runCompile action = do
  tag <- getVarTag
  case runFutharkM tag action of
    Left err -> throwError err
    Right ((), consts, funs, tag') -> do
      putVarTag tag'
      pure $ renderProg tag' $ F.Prog mempty consts funs
  where
    renderProg tag prog =
      renderStrict . layoutPretty defaultLayoutOptions $
        vsep ["name_source" <+> braces (pretty $ getTag tag), pretty prog]

-- | Turn a Remora exp into Futhark.
compileExp :: Exp -> PassM T.Text
compileExp = runCompile . addEntry "main" []

-- | Turn a Remora program into Futhark.
compile :: Prog -> PassM T.Text
compile (Prog decls)
  | null entries = throwError "compile: program has no entry point"
  | Just name <- duplicate =
      throwError $ "compile: duplicate entry point: " <> name
  | otherwise = runCompile $ mapM_ compileDecl decls
  where
    entries = [varName v | Entry v _ _ _ _ _ <- decls]
    duplicate = listToMaybe $ concatMap (drop 1) $ group $ sort entries
