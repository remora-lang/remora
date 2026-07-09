module Futhark (FutharkM, compile, compileExp) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.List (isPrefixOf, singleton, stripPrefix)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Futhark.IR.SOACS qualified as F
import Pass
import Prettyprinter
import Prettyprinter.Render.Text
import Prop
import Syntax hiding (ArrayType, AtomType, Dim, ISpace, Shape, Type)
import Syntax qualified
import Text.Read
import Util
import VName

type Dim = Syntax.Dim VName

type Shape = Syntax.Shape VName

type AtomType = Syntax.AtomType VName

type ArrayType = Syntax.ArrayType VName

type Type = Syntax.Type VName

data Env = Env

data State = State
  { stateStms :: F.Stms F.SOACS,
    stateTag :: Tag,
    stateFuns :: [F.FunDef F.SOACS],
    stateFunBinds :: Map VName (F.FunDef F.SOACS)
  }

newtype FutharkM a = FutharkM {runFutharkM :: StateT State (ReaderT Env (Except Error)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error,
      MonadState State
    )

emit :: F.Stm F.SOACS -> FutharkM ()
emit stm = modify $ \s -> s {stateStms = stateStms s <> F.oneStm stm}

collect :: FutharkM a -> FutharkM (a, F.Stms F.SOACS)
collect m = do
  old_stms <- gets stateStms
  modify $ \s -> s {stateStms = mempty}
  x <- m
  stms <- gets stateStms
  modify $ \s -> s {stateStms = old_stms}
  pure (x, stms)

newVar :: FutharkM F.VName
newVar = do
  tag <- gets stateTag
  modify $ \s -> s {stateTag = nextTag tag}
  pure $ F.VName "v" $ getTag tag

bind :: F.Type -> F.Exp F.SOACS -> FutharkM F.VName
bind t e = do
  v <- newVar
  emit $ F.Let (F.Pat [F.PatElem v t]) (F.defAux ()) e
  pure v

mkBody :: FutharkM [F.SubExp] -> FutharkM (F.Body F.SOACS)
mkBody m = do
  (vs, stms) <- collect m
  pure $ F.Body () stms $ map (F.SubExpRes mempty) vs

addFunction :: F.FunDef F.SOACS -> FutharkM ()
addFunction fun = modify $ \s -> s {stateFuns = stateFuns s ++ [fun]}

-- Assumes the lambda has no free variables.
liftLambda :: [Pat] -> Exp -> AtomType -> FutharkM F.Name
liftLambda params (Array [] (Lambda p body (Info t) _ NE.:| []) _ _) _ =
  liftLambda (params ++ [p]) body t
liftLambda params body t = do
  params' <- mapM compileParam params
  body' <- mkBody $ pure <$> compileExp' body
  ret' <- compileArrayType $ findRet t
  fname <- F.nameFromText . F.prettyText <$> newVar
  addFunction $
    F.FunDef
      { F.funDefEntryPoint = Nothing,
        F.funDefAttrs = mempty,
        F.funDefName = fname,
        F.funDefRetType = [(F.toDecl (F.staticShapes1 ret') F.Nonunique, mempty)],
        F.funDefParams = map (fmap (`F.toDecl` F.Nonunique)) params',
        F.funDefBody = body'
      }
  pure fname

compileAtomType :: AtomType -> FutharkM F.Type
compileAtomType Bool = pure $ F.Prim F.Bool
compileAtomType Int = pure $ F.Prim $ F.IntType F.Int32
compileAtomType Float = pure $ F.Prim $ F.FloatType F.Float32
compileAtomType t = error $ "compileAtomType: unhandled:\n" ++ show t

compileArrayType :: ArrayType -> FutharkM F.Type
compileArrayType (A t shape) = do
  t' <- compileAtomType t
  shape' <- compileShape shape
  pure $ F.arrayOfShape t' shape'
  where
    compileShape :: Shape -> FutharkM F.Shape
    compileShape (ShapeDim d) = F.Shape . pure <$> compileDim d
    compileShape (Concat ds) = mconcat <$> mapM compileShape ds
    compileShape s = error $ "compileShape: unhandled:\n" ++ show s

    compileDim :: Dim -> FutharkM F.SubExp
    compileDim (DimN x) =
      pure $ F.Constant $ F.IntValue $ F.Int64Value $ fromIntegral x
    compileDim d = error $ "compileDim: unhandled:\n" ++ show d

compileType :: Type -> FutharkM F.Type
compileType (Syntax.AtomType t) = compileAtomType t
compileType (Syntax.ArrayType t) = compileArrayType t

compileParam :: Pat -> FutharkM (F.LParam F.SOACS)
compileParam (PatId v _ (Info t) _) = do
  let v' = compileVName v
  t' <- compileArrayType t
  pure $ F.Param mempty v' t'

compileAtom :: Atom -> FutharkM F.SubExp
compileAtom (Base (BoolVal x) _ _) =
  pure $ F.Constant $ F.BoolValue x
compileAtom (Base (IntVal x) _ _) =
  pure $ F.Constant $ F.IntValue $ F.Int32Value $ fromIntegral x
compileAtom (Base (FloatVal x) _ _) =
  pure $ F.Constant $ F.FloatValue $ F.Float32Value x
compileAtom e = error $ "compileAtom: unhandled:\n" ++ show e

compileVName :: VName -> F.VName
compileVName v =
  F.VName (F.nameFromText $ varName v) (getTag $ varTag v)

compileName :: VName -> F.Name
compileName v = F.nameFromText (varName v)

etaExpand :: Exp -> FutharkM (F.Lambda F.SOACS)
etaExpand e@(Var f (Info fty) _) =
  case unrollArrow fty of
    ([], _) -> error $ "etaExpand: not a function type\n" ++ show e
    (paramTys, retTy) -> do
      params <- forM paramTys $ \t -> do
        x <- newVar
        t' <- compileArrayType t
        pure $ F.Param mempty x t'
      retTy' <- compileArrayType retTy
      let args = zipWith (Arg [] . F.Var . F.paramName) params paramTys
      body <- mkBody $ singleton <$> compileApp (compileName f) args retTy
      pure
        F.Lambda
          { F.lambdaParams = params,
            F.lambdaReturnType = [retTy'],
            F.lambdaBody = body
          }
etaExpand e = error $ "etaExpand: unhandled\n" ++ show e

mkSizeSubExp :: Shape -> F.SubExp
mkSizeSubExp (ShapeDim (DimN n)) =
  F.Constant $ F.IntValue $ F.Int64Value $ fromIntegral n
mkSizeSubExp s = error $ "sizeOfShape: unhandled\n" ++ show s

idLambda :: [F.Type] -> FutharkM (F.Lambda F.SOACS)
idLambda ts = do
  params <- forM ts $ \t -> do
    x <- newVar
    pure $ F.Param mempty x t

  body <-
    mkBody $
      pure $
        map (F.Var . F.paramName) params
  pure $
    F.Lambda
      { F.lambdaParams = params,
        F.lambdaReturnType = ts,
        F.lambdaBody = body
      }

compileReduce :: Exp -> Arg -> FutharkM (F.SOAC F.SOACS)
compileReduce op arg = do
  tArg <- (compileAtomType . arrayTypeAtom . argType) arg
  idlam1 <- idLambda [tArg]
  idlam2 <- idLambda [tArg]
  red <- mkReduce
  pure $
    F.Screma
      (mkSizeSubExp $ arrayTypeShape $ argType arg)
      [asVar $ argSExp arg]
      (F.ScremaForm idlam1 [] [red] idlam2)
  where
    asVar (F.Var v) = v
    asVar se = error $ "asVar: unhandled " ++ show se

    mkNeutral (F.Prim pt) = F.Constant $ F.blankPrimValue pt
    mkNeutral t = error $ "mkNeutral: unhandled\n" ++ show t

    mkNeutrals = map mkNeutral . F.lambdaReturnType

    mkReduce :: FutharkM (F.Reduce F.SOACS)
    mkReduce = do
      lam <- etaExpand op
      pure $
        F.Reduce
          { F.redComm = F.Commutative,
            F.redLambda = lam,
            F.redNeutral = mkNeutrals lam
          }

compileFunExp :: Exp -> FutharkM F.Name
compileFunExp (Array [] (Lambda param body (Info t) _ NE.:| []) _ _) =
  liftLambda [param] body t
compileFunExp (Var f _ _) = pure $ F.nameFromText $ varName f
compileFunExp e = error $ "compileFunExp: unhandled\n" ++ show e

binops :: [((T.Text, AtomType), F.BinOp)]
binops =
  [ (("+", Int), F.Add F.Int32 F.OverflowWrap),
    (("-", Int), F.Sub F.Int32 F.OverflowWrap),
    (("f.*", Float), F.FMul F.Float32),
    (("f.+", Float), F.FAdd F.Float32),
    (("f.-", Float), F.FSub F.Float32),
    (("f./", Float), F.FDiv F.Float32),
    (("f.^", Float), F.FPow F.Float32)
  ]

int2Const :: Int -> F.SubExp
int2Const n = F.Constant $ F.IntValue $ F.intValue F.Int32 n

int2Const64 :: Int -> F.SubExp
int2Const64 n = F.Constant $ F.IntValue $ F.intValue F.Int64 n

compileExp' :: Exp -> FutharkM F.SubExp
compileExp' (Array [] (x NE.:| []) _ _) = compileAtom x
compileExp' e@(Array [_] elems (Info (A elem_t _)) _) = do
  elems' <- NE.toList <$> mapM compileAtom elems
  t <- compileType $ typeOf e
  elem_t' <- compileAtomType elem_t
  F.Var <$> bind t (F.BasicOp $ F.ArrayLit elems' elem_t')
compileExp' e@(Frame ds es (Info (A _ _)) _) = do
  es' <- NE.toList <$> mapM compileExp' es
  nestArrayLits ds es' (arrayTypeOf e)
  where
    nestArrayLits :: [Int] -> [F.SubExp] -> ArrayType -> FutharkM F.SubExp
    nestArrayLits [] [e'] _ = pure e'
    nestArrayLits (_ : dims) elems aty = do
      let ess = split elems (product dims)
      ess' <- mapM (\l -> nestArrayLits dims l (peelArrayType aty)) ess
      t' <- compileArrayType aty
      innerT' <- compileArrayType $ peelArrayType aty
      F.Var <$> bind t' (F.BasicOp $ F.ArrayLit ess' innerT')
    nestArrayLits dims elems aty = error $ "unhandled: nestArrayLits: " ++ prettyString dims ++ prettyString elems ++ prettyString aty

    split :: [a] -> Int -> [[a]]
    split [] _ = []
    split as n = take n as : split (drop n as) n
compileExp' (Var v _ _)
  | Just rest <- stripPrefix "iota/" (T.unpack (varName v)),
    Just n <- readMaybe rest =
      compileIota n
  | Just rest <- stripPrefix "undefined-input/" (T.unpack (varName v)),
    Just dims <- mapM readMaybe (splitOn rest 'x') =
      compileRealWorld dims
  | Just rest <- stripPrefix "undefined-weights/" (T.unpack (varName v)),
    Just dims <- mapM readMaybe (splitOn rest 'x') =
      compileRealWorld dims
  where
    compileIota :: Int -> FutharkM F.SubExp
    compileIota n = do
      t <- compileArrayType $ A Int (ShapeDim $ DimN n)
      F.Var <$> bind t (F.BasicOp $ F.Iota (int2Const64 n) (int2Const 0) (int2Const 1) F.Int32)
    compileRealWorld :: [Int] -> FutharkM F.SubExp
    compileRealWorld ds = do
      t <- compileArrayType $ A Float $ Concat $ map (ShapeDim . DimN) ds
      F.Var <$> bind t (F.BasicOp $ F.Replicate (F.Shape $ map int2Const64 ds) (F.Constant $ F.FloatValue $ F.floatValue F.Float32 (37 :: Double)))
compileExp' (Var v _ _) =
  let v' = F.VName (F.nameFromText (varName v)) (getTag (varTag v))
   in pure $ F.Var v'
compileExp' (App (App (Var v _ _) op _ _) arg (Info (t, pframe)) _)
  | Just rest <- stripPrefix "reduce/f/" (T.unpack (varName v)),
    Just (_ :: Int) <- readMaybe rest,
    isScalar op = do
      let ds = intShape pframe
      case arrayTypeOf op of
        A (_ :-> A (argParam :-> _) _) _ -> do
          arg' <- mkArg argParam arg
          t' <- compileArrayType t
          res <-
            withMapNest
              ds
              t
              [arg']
              ( \innerArgs innerT -> case innerArgs of
                  [innerArg] -> do
                    red <- F.Op <$> compileReduce op innerArg
                    innerT' <- compileArrayType innerT
                    F.Var <$> bind innerT' red
                  _ -> error "compileExp': reduce expects a single argument"
              )
          F.Var <$> bind t' (F.BasicOp $ F.SubExp res)
        _ -> error "compileExp': the typechecker has problems, man."
compileExp' e@(App _ _ (Info (t, pframe)) _) = do
  let (f, allArgs) = unrollApp e
  case intShape pframe of
    ds | isScalar (typeOf f) -> do
      let (paramTys, _) = unrollArrow (arrayTypeOf f)
      args <- zipWithM mkArg paramTys allArgs
      fname <- compileFunExp f
      t' <- compileArrayType t
      res <- withMapNest ds t args (compileApp fname)
      F.Var <$> bind t' (F.BasicOp $ F.SubExp res)
    _ -> error $ "compileExp': unhandled lifted apply with func array:\n" ++ show e
compileExp' (Let bs e _ _) =
  mapM_ compileBind bs >> compileExp' e
compileExp' e = error $ "compileExp': unhandled:\n" ++ show e

intDim :: Dim -> Int
intDim =
  runIdentity
    . dimToInt (const $ error "intDim: dimension var")

intShape :: Shape -> [Int]
intShape =
  runIdentity
    . shapeToInts
      (Identity . intDim)
      (const $ error "intShape: shape var")
    . normShape

data Arg
  = Arg
  { argFrame :: [Int],
    argSExp :: F.SubExp,
    argType :: ArrayType
  }
  deriving (Show, Eq)

mkArg :: ArrayType -> Exp -> FutharkM Arg
mkArg t_param x = do
  x' <- compileExp' x
  pure $
    Arg
      { argFrame =
          let argShape = intShape $ shapeOf x
              paramShape = intShape $ arrayTypeShape t_param
           in take (length argShape - length paramShape) argShape,
        argSExp = x',
        argType = arrayTypeOf x
      }

---- Does no lifting
compileApp :: F.Name -> [Arg] -> ArrayType -> FutharkM F.SubExp
compileApp flatten [arg] t
  | "flatten/f/" `isPrefixOf` F.nameToString flatten = do
      let shpInfoMaybe = stripPrefix "flatten/f/" (F.nameToString flatten)
      shpInfo <- case shpInfoMaybe of
        Just s -> pure s
        Nothing -> throwError "unreachable"
      (m, n, c) <- case splitOn shpInfo '-' of
        [m, n, c] -> do
          let maybeList = mapM Text.Read.readMaybe [m, n]
          let maybeCList = mapM Text.Read.readMaybe (splitOn c 'x')
          (m', n') <- case maybeList of
            Just [m', n'] -> pure (m', n')
            _else -> throwError "unreachable"
          cs <- case maybeCList of
            Just cs' -> pure cs'
            Nothing -> case c of
              "_" -> pure []
              _else -> throwError "bad shape in flatten"
          pure (m', n', cs)
        _ -> throwError "bad name of flatten"
      t_arg <- compileArrayType $ argType arg
      arg' <- bind t_arg $ F.BasicOp $ F.SubExp $ argSExp arg
      t' <- compileArrayType t
      F.Var <$> bind t' (F.BasicOp $ F.Reshape arg' $ flattenNewShape (m * n) c)
  where
    flattenNewShape :: Int -> [Int] -> F.NewShape F.SubExp
    flattenNewShape n s =
      F.NewShape
        { F.dimSplices =
            [ F.DimSplice
                0
                2
                (F.Shape {F.shapeDims = [int2Const64 n]})
            ],
          F.newShape = F.Shape {F.shapeDims = int2Const64 n : map int2Const64 s}
        }
compileApp append [arg1, arg2] t
  | "append/f/" `isPrefixOf` F.nameToString append = do
      let shpInfoMaybe = stripPrefix "append/f/" (F.nameToString append)
      shpInfo <- case shpInfoMaybe of
        Just s -> pure s
        Nothing -> throwError "unreachable"
      (m, n) <- case splitOn shpInfo '-' of
        [m, n, _] -> do
          let maybeList = mapM Text.Read.readMaybe [m, n]
          case maybeList of
            Just [m', n'] -> pure (m', n')
            _else -> throwError "unreachable"
        _ -> throwError "bad name of append"
      t_arg1 <- compileArrayType $ argType arg1
      t_arg2 <- compileArrayType $ argType arg2
      t' <- compileArrayType t
      arg1' <- bind t_arg1 $ F.BasicOp $ F.SubExp $ argSExp arg1
      arg2' <- bind t_arg2 $ F.BasicOp $ F.SubExp $ argSExp arg2
      F.Var <$> bind t' (F.BasicOp $ F.Concat 0 (arg1' NE.:| [arg2']) (int2Const64 (m + n)))
compileApp fName [arg] t
  | "transpose2d/f/" `isPrefixOf` F.nameToString fName = do
      t' <- compileArrayType t
      t_arg <- compileArrayType $ argType arg
      arg' <- bind t_arg $ F.BasicOp $ F.SubExp $ argSExp arg
      F.Var <$> bind t' (F.BasicOp $ F.Rearrange arg' [1, 0])
compileApp fName [arg] t
  | "replicate/" `isPrefixOf` F.nameToString fName = do
      t' <- compileArrayType t
      let nameStr = F.nameToString fName
          fDims = do
            shpInfo <- stripPrefix "replicate/" nameStr
            (_, rest) <- case shpInfo of
              ('i' : '/' : r) -> Just ((), r)
              ('f' : '/' : r) -> Just ((), r)
              _ -> Nothing
            fStr <- case splitOn rest '-' of
              [f, _] -> Just f
              _ -> Nothing
            let parseShape "_" = Just []
                parseShape sh = mapM readMaybe (splitOn sh 'x')
            parseShape fStr
      case fDims of
        Just dims -> F.Var <$> bind t' (F.BasicOp $ F.Replicate (F.Shape $ map int2Const64 dims) $ argSExp arg)
        Nothing -> error $ "replicate: failed to parse shape from " ++ nameStr
compileApp fName [arr, idx] t
  | "index2d/f/" `isPrefixOf` F.nameToString fName = do
      t' <- compileArrayType t
      t_arr <- compileArrayType $ argType arr
      t_idx <- compileArrayType $ argType idx
      arr' <- bind t_arr $ F.BasicOp $ F.SubExp $ argSExp arr
      idx' <- bind t_idx $ F.BasicOp $ F.SubExp $ argSExp idx
      singleIdxType <- compileArrayType $ A Int (Concat [])
      let singleIdxType64 = F.arrayOfShape (F.Prim $ F.IntType F.Int64) (F.Shape [])
      i <- F.Var <$> bind singleIdxType (F.BasicOp $ F.Index idx' $ F.Slice [F.DimFix $ int2Const64 0])
      j <- F.Var <$> bind singleIdxType (F.BasicOp $ F.Index idx' $ F.Slice [F.DimFix $ int2Const64 1])
      i64 <- F.Var <$> bind singleIdxType64 (F.BasicOp $ F.ConvOp (F.SExt F.Int32 F.Int64) i)
      j64 <- F.Var <$> bind singleIdxType64 (F.BasicOp $ F.ConvOp (F.SExt F.Int32 F.Int64) j)
      F.Var <$> bind t' (F.BasicOp $ F.Index arr' $ F.Slice [F.DimFix i64, F.DimFix j64])
compileApp bop [arg_x, arg_y] t
  | Just op <- lookup (F.nameToText bop, arrayTypeAtom t) binops = do
      t' <- compileArrayType t
      F.Var <$> bind t' (F.BasicOp $ F.BinOp op (argSExp arg_x) (argSExp arg_y))
compileApp f args t = do
  t' <- compileArrayType t
  F.Var <$> bind t' (F.Apply f (map ((,F.Observe) . argSExp) args) [(F.staticShapes1 $ F.toDecl t' F.Nonunique, mempty)] F.Safe)

withMapNest ::
  [Int] ->
  ArrayType ->
  [Arg] ->
  ([Arg] -> ArrayType -> FutharkM F.SubExp) ->
  FutharkM F.SubExp
withMapNest [] t args m = m args t
withMapNest (d : ds) t args m = do
  argPairs' <- mapM mapArg args
  let (mapArgs, args') = first catMaybes $ unzip argPairs'
      paramArgs = concatMap (\(l, r) -> [r | isJust l]) argPairs'
  params <-
    forM paramArgs $ \a -> do
      param_t <- compileArrayType $ argType a
      pure $ F.Param mempty (sexpToVName $ argSExp a) param_t
  let t_body = peelArrayType t
  t_body' <- compileArrayType t_body
  let recRes = withMapNest ds t_body args' m
  body <- mkBody (singleton <$> recRes)
  t' <- compileArrayType t
  idlam <- idLambda [t_body']
  F.Var
    <$> bind
      t'
      ( F.Op
          $ F.Screma
            (F.Constant (F.IntValue (F.Int64Value (fromIntegral d))))
            (map (sexpToVName . argSExp) mapArgs)
          $ F.ScremaForm
            F.Lambda
              { F.lambdaParams = params,
                F.lambdaReturnType = [t_body'],
                F.lambdaBody = body
              }
            []
            []
            idlam
      )
  where
    mapArg :: Arg -> FutharkM (Maybe Arg, Arg)
    mapArg arg@(Arg [] (F.Var _) _) = pure (Nothing, arg)
    mapArg (Arg [] se aty) = do
      t' <- compileArrayType aty
      se' <- F.Var <$> bind t' (F.BasicOp $ F.SubExp se)
      pure (Nothing, Arg [] se' aty)
    mapArg arg@(Arg (_ : fs) _ aty) = do
      se' <- F.Var <$> newVar
      pure (Just arg, Arg fs se' $ peelArrayType aty)

    sexpToVName :: F.SubExp -> F.VName
    sexpToVName (F.Var vname) = vname
    sexpToVName se = error $ "shouldn't ever happen" ++ show se

compileBind :: Bind -> FutharkM ()
compileBind BindType {} = pure ()
compileBind BindISpace {} = pure ()
compileBind (BindFun f params _ body (Info ret) _) = do
  params' <- mapM compileParam $ NE.toList params
  body' <- mkBody $ pure <$> compileExp' body
  ret' <- compileArrayType $ findRet ret
  let mkEntryParam p =
        F.EntryParam
          { F.entryParamName = F.baseName (F.paramName p),
            F.entryParamUniqueness = F.Nonunique,
            F.entryParamType = F.TypeTransparent $ valueType (F.paramType p)
          }
  let entryResult =
        F.EntryResult
          { F.entryResultUniqueness = F.Nonunique,
            F.entryResultType = F.TypeTransparent $ valueType ret'
          }
  let fun =
        F.FunDef
          { F.funDefEntryPoint = Just (F.nameFromText $ varName f, map mkEntryParam params', entryResult, Nothing),
            F.funDefAttrs = mempty,
            F.funDefName = F.nameFromText $ varName f,
            F.funDefRetType = [(F.toDecl (F.staticShapes1 ret') F.Nonunique, mempty)],
            F.funDefParams = map (fmap (`F.toDecl` F.Nonunique)) params',
            F.funDefBody = body'
          }
  addFunction fun
  modify $
    \s -> s {stateFunBinds = M.insert f fun $ stateFunBinds s}
compileBind (BindVal v _ e _) = do
  t <- compileArrayType $ arrayTypeOf e
  e' <- F.BasicOp . F.SubExp <$> compileExp' e
  let v' = F.VName (F.nameFromText (varName v)) (getTag (varTag v))
  emit $ F.Let (F.Pat [F.PatElem v' t]) (F.defAux ()) e'
compileBind b = error $ "compileBind: unhandled " ++ show b

valueType :: F.Type -> F.ValueType
valueType (F.Prim pt) =
  F.ValueType F.Signed mempty pt
valueType (F.Array pt shape _) =
  F.ValueType F.Signed (F.Rank (F.shapeRank shape)) pt
valueType t = error $ "valueType: unhandled " ++ show t

wrapInMain :: (F.SubExp, F.Type) -> State -> T.Text
wrapInMain (e, ret) (State stms counter funs _) =
  renderStrict . layoutPretty defaultLayoutOptions $
    vsep
      [ "name_source" <+> braces (pretty counter),
        pretty $
          F.Prog
            { F.progTypes = mempty,
              F.progConsts = mempty,
              F.progFuns =
                funs
                  ++ [ F.FunDef
                         { F.funDefEntryPoint =
                             Just
                               ( "main",
                                 [],
                                 F.EntryResult F.Nonunique $
                                   F.TypeTransparent $
                                     valueType ret,
                                 Nothing
                               ),
                           F.funDefAttrs = mempty,
                           F.funDefName = "entry_main",
                           F.funDefRetType = [(F.toDecl (F.staticShapes1 ret) F.Nonunique, mempty)],
                           F.funDefParams = [],
                           F.funDefBody = F.Body () stms [F.SubExpRes mempty e]
                         }
                     ]
            }
      ]

runCompile :: FutharkM (F.SubExp, F.Type) -> PassM T.Text
runCompile action = do
  tag <- getVarTag
  let initialState = State mempty tag mempty mempty
      result =
        runExcept
          (runReaderT (runStateT (runFutharkM action) initialState) Env)
  case result of
    Left err -> throwError err
    Right (res, s) -> do
      putVarTag $ stateTag s
      pure $ wrapInMain res s

-- | Turn a Remora exp into Futhark.
compileExp :: Exp -> PassM T.Text
compileExp e =
  runCompile $
    (,) <$> compileExp' e <*> compileType (typeOf e)

-- | Turn a Remora program into Futhark.
compile :: Prog -> PassM T.Text
compile (Prog decls) =
  case [body | Entry v _ _ body _ _ <- decls, varName v == "main"] of
    [] -> throwError "compile: program has no main entry"
    [body] ->
      runCompile $ do
        mapM_ compileDecl decls
        (,) <$> compileExp' body <*> compileType (typeOf body)
    _ -> throwError "compile: multiple main entries"

compileDecl :: Decl -> FutharkM ()
compileDecl (Def b) = compileBind b
compileDecl Entry {} = pure ()
