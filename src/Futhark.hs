module Futhark (FutharkM, compile) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.List (singleton)
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Debug.Trace
import Futhark.IR.GPU (mapSOAC)
import Futhark.IR.SOACS qualified as F
import Prettyprinter
import Prettyprinter.Render.Text
import Prop
import RemoraPrelude (Prelude)
import Substitute
import Syntax hiding (ArrayType, Atom, AtomType, Bind, Dim, Exp, Extent, Pat, Shape, Type)
import Syntax qualified
import Util
import VName

type Dim = Syntax.Dim VName

type Exp = Syntax.Exp Info VName

type Atom = Syntax.Atom Info VName

type Shape = Syntax.Shape VName

type AtomType = Syntax.AtomType VName

type ArrayType = Syntax.ArrayType VName

type Type = Syntax.Type VName

type TypeExp = Syntax.TypeExp VName

type Pat = Syntax.Pat Info VName

type Bind = Syntax.Bind Info VName

type Extent = Syntax.Extent VName

type Error = T.Text

data Env = Env

data State = State
  { stateStms :: F.Stms F.SOACS,
    stateCounter :: Int,
    stateFuns :: [F.FunDef F.SOACS],
    stateFunBinds :: Map VName (F.FunDef F.SOACS)
  }

-- | The Futhark generator monad.
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
  x <- gets stateCounter
  modify $ \s -> s {stateCounter = succ x}
  pure $ F.VName "v" x

binds :: [F.Type] -> F.Exp F.SOACS -> FutharkM [F.VName]
binds ts e = do
  vs <- mapM (const newVar) ts
  let p = F.Pat $ zipWith F.PatElem vs ts
  emit $ F.Let p (F.defAux ()) e
  pure vs

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

findRet :: AtomType -> ArrayType
findRet (_ :-> t) = t
findRet t = error $ "findRet: unhandled:\n" ++ show t

-- Assumes the lambda has no free variables.
liftLambda :: [Pat] -> Exp -> AtomType -> FutharkM F.Name
liftLambda params body t = do
  params' <- mapM compileParam params
  body' <- mkBody $ pure <$> compileExp body
  ret' <- compileArrayType $ findRet t
  fname <- F.nameFromText . F.prettyText <$> newVar
  addFunction $
    F.FunDef
      { F.funDefEntryPoint = Nothing,
        F.funDefAttrs = mempty,
        F.funDefName = fname,
        F.funDefRetType = [(F.toDecl (F.staticShapes1 ret') F.Nonunique, mempty)],
        F.funDefParams = map (fmap (flip F.toDecl F.Nonunique)) params',
        F.funDefBody = body'
      }
  pure fname

compileDim :: Dim -> FutharkM F.SubExp
compileDim (DimN x) = pure $ F.Constant $ F.IntValue $ F.Int64Value $ fromIntegral x
compileDim d = error $ "compileDim: unhandled:\n" ++ show d

compileShape :: Shape -> FutharkM F.Shape
compileShape (ShapeDim d) = F.Shape . pure <$> compileDim d
compileShape (Concat ds) = mconcat <$> mapM compileShape ds
compileShape s = error $ "compileShape: unhandled:\n" ++ show s

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

compileType :: Type -> FutharkM F.Type
compileType (Syntax.AtomType t) = compileAtomType t
compileType (Syntax.ArrayType t) = compileArrayType t

compileParam :: Pat -> FutharkM (F.LParam F.SOACS)
compileParam (PatId v _ (Info t) _) = do
  let v' = compileVName v
  t' <- compileArrayType t
  pure $ F.Param mempty v' t'

compileAtom :: Atom -> FutharkM (F.SubExp)
compileAtom (Base (BoolVal x) _ _) =
  pure $ F.Constant $ F.BoolValue x
compileAtom (Base (IntVal x) _ _) =
  pure $ F.Constant $ F.IntValue $ F.Int32Value $ fromIntegral x
compileAtom (Base (FloatVal x) _ _) =
  pure $ F.Constant $ F.FloatValue $ F.Float32Value x
compileAtom e = error $ "compileAtom: unhandled:\n" ++ show e

lookupFun :: VName -> FutharkM (F.FunDef F.SOACS)
lookupFun f = do
  mdef <- gets $ (M.!? f) . stateFunBinds
  case mdef of
    Nothing -> error $ "lookupFun: unknown fun\n" ++ show f
    Just def -> pure def

compileVName :: VName -> F.VName
compileVName v =
  F.VName (F.nameFromText (varName v)) (getTag (varTag v))

compileName :: VName -> F.Name
compileName v = F.nameFromText (varName v)

etaExpand :: Exp -> FutharkM (F.Lambda F.SOACS)
etaExpand e@(Var f (Info (A (ts :-> r) s)) _)
  | s @= mempty = do
      case (lookup (varName f, arrayTypeAtom r) binops, ts) of
        (Just op, [t_x, t_y]) -> do
          x <- newVar
          y <- newVar
          t_x' <- compileArrayType t_x
          t_y' <- compileArrayType t_y
          r' <- compileArrayType r
          let params = [F.Param mempty x t_x', F.Param mempty y t_y']
              args = [(F.Var x, F.Observe), (F.Var y, F.Observe)]

          body <-
            mkBody $
              (pure . F.Var) <$> bind r' (F.BasicOp (F.BinOp op (F.Var x) (F.Var y)))

          pure
            F.Lambda
              { F.lambdaParams = params,
                F.lambdaReturnType = [r'],
                F.lambdaBody = body
              }
        (Nothing, _) -> do
          def <- lookupFun f
          params <- forM (F.funDefParams def) $ \(F.Param a v t) -> do
            x <- newVar
            pure $ F.Param a x t

          let args = map ((,F.Observe) . F.Var . F.paramName) params
              mrt = mapM (F.hasStaticShape . F.fromDecl . fst) $ F.funDefRetType def
              rt =
                case mrt of
                  Nothing -> error $ "etaExpand: not static shape:\n" ++ show e
                  Just rt' -> rt'
          body <-
            mkBody $
              (map F.Var)
                <$> binds rt (F.Apply (compileName f) args (F.funDefRetType def) F.Unsafe)
          pure
            F.Lambda
              { F.lambdaParams = (fmap . fmap) F.fromDecl params,
                F.lambdaReturnType = rt,
                F.lambdaBody = body
              }
etaExpand e = error $ "etaExpand: unhandled\n" ++ show e

mkSizeSubExp :: Shape -> F.SubExp
mkSizeSubExp (ShapeDim (DimN n)) =
  F.Constant $ F.IntValue $ F.Int64Value $ fromIntegral n
mkSizeSubExp s = error $ "sizeOfShape: unhandled\n" ++ show s

compileReduce :: Exp -> [Exp] -> FutharkM (F.SOAC F.SOACS)
compileReduce op xss = do
  xss' <- traverse compileExp xss
  ts <- mapM (compileAtomType . arrayTypeAtom . arrayTypeOf) xss
  idlam <- idLambda ts
  red <- mkReduce
  pure
    $ F.Screma
      (mkSizeSubExp $ shapeOf $ head xss)
      (map asVar xss')
    $ F.redomapSOAC [red] idlam
  where
    asVar (F.Var v) = v

    mkNeutral (F.Prim pt) = F.Constant $ F.blankPrimValue pt
    mkNeutral t = error $ "mkNeutral: unhandled\n" ++ show t

    mkNeutrals = map mkNeutral . F.lambdaReturnType

    idLambda :: [F.Type] -> FutharkM (F.Lambda F.SOACS)
    idLambda ts = do
      params <- forM ts $ \t -> do
        x <- newVar
        pure $ F.Param mempty x t
      stms <- gets stateStms

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
compileFunExp (Array [] [(Lambda params body (Info t) _)] _ _) =
  liftLambda params body t
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

compileExp :: Exp -> FutharkM F.SubExp
compileExp (Array [] [x] _ _) = compileAtom x
compileExp e@(Array [_] elems (Info (A elem_t _)) _) = do
  elems' <- mapM compileAtom elems
  t <- compileType $ typeOf e
  elem_t' <- compileAtomType elem_t
  F.Var <$> bind t (F.BasicOp $ F.ArrayLit elems' elem_t')
compileExp e@(Frame _ es (Info (A elem_t _)) _) = do
  es' <- mapM compileExp es
  t <- compileType $ typeOf e
  elem_t' <- compileAtomType elem_t
  F.Var <$> bind t (F.BasicOp $ F.ArrayLit es' elem_t')
-- compile iotas
compileExp (Var v _ _)
  | varName v == "iota/3" = compileIota 3
  | varName v == "iota/608" = compileIota 608
  where
    compileIota :: Int -> FutharkM F.SubExp
    compileIota n = do
      t <- compileArrayType $ A Int (ShapeDim $ DimN n)
      F.Var <$> bind t (F.BasicOp $ F.Iota (int2Const n) (int2Const 0) (int2Const 1) F.Int32)
compileExp (Var v _ _) =
  let v' = F.VName (F.nameFromText (varName v)) (getTag (varTag v))
   in pure $ F.Var v'
compileExp e@(App f xs (Info (t, pframe)) _) = do
  case intShape $ normShape pframe of
    ds | null $ intShape $ normShape $ shapeOf $ typeOf f -> do
      let (A (params :-> res) _) = arrayTypeOf f
      args <- zipWithM mkArg params xs
      fname <- compileFunExp f
      F.Var <$> withMapNest ds t args (compileApp fname)
    _ -> error $ "compileExp: unhandled lifted apply with func array:\n" ++ show e
  where
    mkArg t_param x = do
      x' <- compileExp x
      pure $
        Arg
          { argFrame =
              let argShape = intShape $ normShape $ shapeOf x
                  paramShape = intShape $ normShape $ arrayTypeShape $ t_param
               in take (length argShape - length paramShape) argShape,
            argSExp = x',
            argType = arrayTypeOf x
          }
    asVar (F.Var v) = v

    intDim (DimVar d) = error "intDim: AAAAAAAAAAAAAAAAAAA"
    intDim (DimN d) = d
    intDim (Add ds) = sum $ map intDim ds

    intShape :: Shape -> [Int]
    intShape (ShapeVar s) = error "intShape: AAAAAAAAAAAAAAAAAAA"
    intShape (ShapeDim d) = singleton $ intDim d
    intShape (Concat ss) = concatMap intShape ss
compileExp (Let bs e _ _) =
  mapM compileBind bs >> compileExp e
compileExp e = error $ "compileExp: unhandled:\n" ++ show e

---- Does no lifting
compileApp :: F.Name -> [Arg] -> ArrayType -> FutharkM F.VName
---- compile all the prelude stuff
-- compileExp (App (Var v _ _) (op : xss) (Info (t, pframe)) _)
--  | "f.reduce3" == varName v = do
--      red <- compileReduce op xss
--      t' <- compileArrayType t
--      F.Var <$> bind t' (F.Op red)
--  | "reduce/f/26" == varName v = do
--      red <- compileReduce op xss
--      t' <- compileArrayType t
--      F.Var <$> bind t' (F.Op red)
-- compile flattens
compileApp flatten [arg] t
  | Just newShape <- lookup (F.nameToText flatten) flattenVersions = do
      t_arg <- compileArrayType $ argType arg
      arg' <- bind t_arg $ F.BasicOp $ F.SubExp $ argSExp arg
      t' <- compileArrayType t
      bind t' $ F.BasicOp $ F.Reshape arg' newShape
  where
    -- maps a flatten version from prelude to output shape
    flattenVersions :: [(T.Text, F.NewShape F.SubExp)]
    flattenVersions =
      [ ("flatten/f/3-9-32", flattenNewShape 27 [32]),
        ("flatten/f/3-3-32", flattenNewShape 9 [32]),
        ("flatten/f/3-9-_", flattenNewShape 27 []),
        ("flatten/f/3-3-_", flattenNewShape 9 []),
        ("flatten/f/608-608-27", flattenNewShape (608 * 608) [27])
      ]

    flattenNewShape :: Int -> [Int] -> F.NewShape F.SubExp
    flattenNewShape n s =
      F.NewShape
        { F.dimSplices =
            [ F.DimSplice
                0
                2
                (F.Shape {F.shapeDims = [int2Const n]})
            ],
          F.newShape = F.Shape {F.shapeDims = (int2Const n) : (map int2Const s)}
        }

-- compile appends
compileApp append [arg1, arg2] t
  | Just (dimId, resLen) <- lookup (F.nameToText append) appendVersions = do
      t_arg1 <- compileArrayType $ argType arg1
      t_arg2 <- compileArrayType $ argType arg2
      t' <- compileArrayType t
      arg1' <- bind t_arg1 $ F.BasicOp $ F.SubExp $ argSExp arg1
      arg2' <- bind t_arg2 $ F.BasicOp $ F.SubExp $ argSExp arg2
      bind t' (F.BasicOp $ F.Concat dimId (arg1' NE.:| [arg2']) resLen)
  where
    -- map append version from prelude to (dim along which to append, resuling size of that dim)
    appendVersions :: [(T.Text, (Int, F.SubExp))]
    appendVersions =
      [ ("append/f/608-1-_", (0, int2Const 609)),
        ("append/f/1-609-_", (0, int2Const 610)),
        ("append/f/608-1-610", (0, int2Const 609)),
        ("append/f/1-609-610", (0, int2Const 610))
      ]
compileApp "transpose/f/27-32" [arg] t = do
  t' <- compileArrayType t
  t_arg <- compileArrayType $ argType arg
  arg' <- bind t_arg $ F.BasicOp $ F.SubExp $ argSExp arg
  bind t' (F.BasicOp $ F.Rearrange arg' [1, 0])
compileApp "index2d/f/610" [arr, idx] t = do
  t' <- compileArrayType t
  t_arr <- compileArrayType $ argType arr
  t_idx <- compileArrayType $ argType idx
  arr' <- bind t_arr $ F.BasicOp $ F.SubExp $ argSExp arr
  idx' <- bind t_idx $ F.BasicOp $ F.SubExp $ argSExp idx
  singleIdxType <- compileArrayType $ A Int (Concat [])
  i <- F.Var <$> bind singleIdxType (F.BasicOp $ F.Index idx' $ F.Slice [F.DimFix $ int2Const 0])
  j <- F.Var <$> bind singleIdxType (F.BasicOp $ F.Index idx' $ F.Slice [F.DimFix $ int2Const 1])
  bind t' (F.BasicOp $ F.Index arr' $ F.Slice [F.DimFix i, F.DimFix j])
-- compile binary ops
compileApp bop [arg_x, arg_y] t
  | Just op <- lookup (F.nameToText bop, arrayTypeAtom t) binops = do
      t' <- compileArrayType t
      bind t' $ F.BasicOp $ F.BinOp op (argSExp arg_x) (argSExp arg_y)

data Arg
  = Arg
  { argFrame :: [Int],
    argSExp :: F.SubExp,
    argType :: ArrayType
  }
  deriving (Show, Eq)

withMapNest ::
  [Int] ->
  ArrayType ->
  [Arg] ->
  ([Arg] -> ArrayType -> FutharkM F.VName) ->
  FutharkM F.VName
withMapNest [] t args m = m args t
withMapNest (d : ds) t args m = do
  (mapArgs, args') <- (first catMaybes . unzip) <$> mapM mapArg args
  params <-
    forM mapArgs $ \a -> do
      param_t <- compileArrayType $ argType a
      pure $ F.Param mempty (sexpToVName $ argSExp a) param_t
  let t_body = peelArrayType t
  body <- mkBody $ (pure . F.Var) <$> withMapNest ds t_body args' m
  t' <- compileArrayType t
  t_body' <- compileArrayType t_body
  bind t'
    $ F.Op
    $ F.Screma
      (F.Constant (F.IntValue (F.Int64Value (fromIntegral d))))
      (map (sexpToVName . argSExp) mapArgs)
    $ F.mapSOAC
      F.Lambda
        { F.lambdaParams = params,
          F.lambdaReturnType = [t_body'],
          F.lambdaBody = body
        }
  where
    mapArg :: Arg -> FutharkM (Maybe Arg, Arg)
    mapArg arg@(Arg [] _ _) = pure (Nothing, arg)
    mapArg arg@(Arg (f : fs) se t) = do
      se <- F.Var <$> newVar
      pure $ (Just arg, Arg fs se $ peelArrayType t)
    mapArg arg =
      error $ "mapArg: " <> show arg

    sexpToVName :: F.SubExp -> F.VName
    sexpToVName (F.Var vname) = vname
    sexpToVName se = error "shouldn't ever happen"

    peelArrayType :: ArrayType -> ArrayType
    peelArrayType (A a s) = A a $ peelShape s
    peelArrayType t = t

    peelShape :: Shape -> Shape
    peelShape = peelShape' . normShape
      where
        peelShape' (Concat (s : ss)) = Concat ss
        peelShape' _ = mempty

compileBind :: Bind -> FutharkM ()
compileBind BindType {} = pure ()
compileBind BindExtent {} = pure ()
compileBind (BindFun f params _ body (Info ret) _) = do
  params' <- mapM compileParam params
  body' <- mkBody $ pure <$> compileExp body
  ret' <- compileArrayType $ findRet ret
  let fun =
        F.FunDef
          { F.funDefEntryPoint = Nothing,
            F.funDefAttrs = mempty,
            F.funDefName = F.nameFromText $ varName f,
            F.funDefRetType = [(F.toDecl (F.staticShapes1 ret') F.Nonunique, mempty)],
            F.funDefParams = map (fmap (flip F.toDecl F.Nonunique)) params',
            F.funDefBody = body'
          }
  addFunction fun
  modify $
    \s -> s {stateFunBinds = M.insert f fun $ stateFunBinds s}
compileBind (BindVal v _ e _) = do
  t <- compileArrayType $ arrayTypeOf e
  e' <- (F.BasicOp . F.SubExp) <$> compileExp e
  let v' = F.VName (F.nameFromText (varName v)) (getTag (varTag v))
  emit $ F.Let (F.Pat [F.PatElem v' t]) (F.defAux ()) e'
compileBind b = error $ "compileBind: unhandled " ++ show b

valueType :: F.Type -> F.ValueType
valueType (F.Prim pt) =
  F.ValueType F.Signed mempty pt
valueType (F.Array pt shape _) =
  F.ValueType F.Signed (F.Rank (F.shapeRank shape)) pt
valueType t = error $ "valueType: unhandled " ++ show t

wrapInMain :: ((F.SubExp, F.Type), State) -> T.Text
wrapInMain ((e, ret), State stms counter funs _) =
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
                                 [ F.EntryResult F.Nonunique $
                                     F.TypeTransparent $
                                       valueType ret
                                 ]
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

-- | Turn Remora into Futhark.
compile :: Prelude VName FutharkM -> Exp -> Either Error T.Text
compile _prelude e =
  wrapInMain
    <$> runExcept
      ( runReaderT
          ( runStateT
              ( runFutharkM
                  ( (,)
                      <$> compileExp e
                      <*> compileType (typeOf e)
                  )
              )
              initialState
          )
          Env
      )
  where
    initialState = State mempty 0 mempty mempty
