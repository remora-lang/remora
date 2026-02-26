module Futhark (FutharkM, compile) where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Debug.Trace
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

map1 :: F.BinOp -> Int -> ArrayType -> [F.VName] -> F.Type -> FutharkM F.VName
map1 op dim (A (pts :-> r) shape) xss@[_, _] t
  | shape @= mempty = do
      x' <- newVar
      y' <- newVar
      pts' <- mapM compileArrayType pts
      r' <- compileArrayType r
      body <-
        mkBody $
          pure . F.Var
            <$> bind r' (F.BasicOp (F.BinOp op (F.Var x') (F.Var y')))
      bind t
        $ F.Op
        $ F.Screma
          (F.Constant (F.IntValue (F.Int64Value (fromIntegral dim))))
          xss
        $ F.mapSOAC
        $ F.Lambda
          [F.Param mempty x' (pts' !! 0), F.Param mempty y' (pts' !! 1)]
          [r']
          body
map1 _ t _ xss _ = error $ "map1: unhandled\n" ++ show t ++ "\n" ++ show xss

rep :: F.VName -> ArrayType -> Shape -> FutharkM F.VName
rep x t s
  | s @= mempty = pure x
  | otherwise = do
      s' <- compileShape s
      t' <- compileArrayType $ A (arrayTypeAtom t) (s <> arrayTypeShape t)
      bind t' $ F.BasicOp $ F.Replicate s' (F.Var x)

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

compileBinOp :: F.BinOp -> ArrayType -> Exp -> Exp -> FutharkM F.SubExp
compileBinOp op t x y = do
  x' <- compileExp x
  y' <- compileExp y
  t' <- compileArrayType t
  F.Var <$> bind t' (F.BasicOp (F.BinOp op x' y'))
compileBinOp v _ _ _ = error $ "compileBinOp: unhandled\n" ++ show v

compileExp :: Exp -> FutharkM F.SubExp
compileExp (Array [] [x] _ _) = compileAtom x
compileExp e@(Array [_] elems (Info (A elem_t _)) _) = do
  elems' <- mapM compileAtom elems
  t <- compileType $ typeOf e
  elem_t' <- compileAtomType elem_t
  F.Var <$> bind t (F.BasicOp $ F.ArrayLit elems' elem_t')
compileExp (Var v _ _) =
  let v' = F.VName (F.nameFromText (varName v)) (getTag (varTag v))
   in pure $ F.Var v'
compileExp (App (Var v _ _) (op : xss) (Info (t, pframe)) _)
  | "f.reduce3" == varName v = do
      red <- compileReduce op xss
      t' <- compileArrayType t
      F.Var <$> bind t' (F.Op red)
compileExp e@(App f@(Var v _ _) [x, y] (Info (t, pframe)) _)
  | Just op <- lookup (varName v, arrayTypeAtom t) binops =
      case pframe of
        ShapeDim (DimN n) -> do
          x' <- compileExp x
          y' <- compileExp y
          x'' <- rep (asVar x') (arrayTypeOf x) $ fromMaybe mempty (pframe \\ shapeOf x)
          y'' <- rep (asVar y') (arrayTypeOf y) $ fromMaybe mempty (pframe \\ shapeOf y)
          F.Var <$> (map1 op (fromIntegral n) (arrayTypeOf f) [x'', y''] =<< compileArrayType t)
        Concat [] -> compileBinOp op t x y
        _ -> error $ "compileExp: unhandled\n" ++ show e
  where
    asVar (F.Var v) = v
compileExp e@(App f xs (Info (t, pframe)) _) = do
  f' <- compileFunExp f
  xs' <- traverse compileExp xs
  t' <- compileArrayType t
  case intShape $ normShape pframe of
    [] ->
      fmap F.Var . bind t' $
        F.Apply
          f'
          (map (,F.Observe) xs')
          [(F.staticShapes1 $ flip F.toDecl F.Nonunique t', mempty)]
          F.Safe
    _ -> error $ "compileExp: unhandled:\n" ++ show e
  where
    asVar (F.Var v) = v

    intDim :: Dim -> Int
    intDim (DimVar d) = error "intDim: AAAAAAAAAAAAAAAAAAA"
    intDim (DimN d) = fromIntegral d
    intDim (Add ds) = sum $ map intDim ds

    intShape :: Shape -> [Int]
    intShape (ShapeVar s) = error "intShape: AAAAAAAAAAAAAAAAAAA"
    intShape (ShapeDim d) = pure $ intDim d
    intShape (Concat ss) = concat $ map intShape ss
compileExp (Let bs e _ _) =
  mapM compileBind bs >> compileExp e
compileExp e = error $ "compileExp: unhandled:\n" ++ show e

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
