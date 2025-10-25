module Futhark (FutharkM, compile) where

import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Except
import Data.Text qualified as T
import Futhark.IR.SOACS qualified as F
import Prettyprinter
import Prettyprinter.Render.Text
import RemoraPrelude (Prelude)
import Syntax hiding (Atom, Dim, Exp, Idx, Shape, Type, (\\))
import Syntax qualified
import VName

type Dim = Syntax.Dim VName

type Exp = Syntax.Exp Info VName

type Atom = Syntax.Atom Info VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

data Env = Env

type Error = T.Text

data State = State
  { stateStms :: F.Stms F.SOACS,
    stateCounter :: Int,
    stateFuns :: [F.FunDef F.SOACS]
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
  x <- m
  stms <- gets stateStms
  modify $ \s -> s {stateStms = old_stms}
  pure (x, stms)

newVar :: FutharkM F.VName
newVar = do
  x <- gets stateCounter
  modify $ \s -> s {stateCounter = succ x}
  pure $ F.VName "v" x

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

findRet :: Type -> Type
findRet (_ :-> t) = findRet t
findRet t = t

-- Assumes the lambda has no free variables.
liftLambda :: [(VName, Type)] -> Exp -> Type -> FutharkM F.Name
liftLambda params body ret = do
  params' <- mapM compileParam params
  body' <- mkBody $ pure <$> compileExp body
  ret' <- compileType $ findRet ret
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

compileType :: Type -> FutharkM F.Type
compileType Bool = pure $ F.Prim F.Bool
compileType Int = pure $ F.Prim $ F.IntType F.Int32
compileType Float = pure $ F.Prim $ F.FloatType F.Float64
compileType (TArr t shape) = do
  t' <- compileType t
  shape' <- compileShape shape
  pure $ F.arrayOfShape t' shape'
compileType t = error $ "compileType: unhandled:\n" ++ show t

compileParam :: (VName, Type) -> FutharkM (F.LParam F.SOACS)
compileParam (v, t) = do
  let v' = F.VName (F.nameFromText (varName v)) (getTag (varTag v))
  t' <- compileType t
  pure $ F.Param mempty v' t'

compileAtom :: Atom -> FutharkM (F.SubExp)
compileAtom (Base (BoolVal x) _ _) =
  pure $ F.Constant $ F.BoolValue x
compileAtom (Base (IntVal x) _ _) =
  pure $ F.Constant $ F.IntValue $ F.Int32Value $ fromIntegral x
compileAtom (Base (FloatVal x) _ _) =
  pure $ F.Constant $ F.FloatValue $ F.Float32Value x
compileAtom e = error $ "compileAtom: unhandled:\n" ++ show e

map1 :: Int -> Type -> F.Name -> [F.VName] -> F.Type -> FutharkM F.VName
map1 dim (pts :-> r) f xss@[_, _] t = do
  x' <- newVar
  y' <- newVar
  pts' <- mapM compileType pts
  r' <- compileType r
  body <-
    mkBody $
      pure . F.Var
        <$> bind r' (F.BasicOp (F.BinOp (F.Add F.Int32 F.OverflowWrap) (F.Var x') (F.Var y')))
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

compileFunExp :: Exp -> FutharkM F.Name
compileFunExp (Array [] [(Lambda params body (Info t) _)] _ _) =
  liftLambda params body t
compileFunExp e = pure $ error $ "compileFunExp: unhandled\n" ++ show e

compileExp :: Exp -> FutharkM F.SubExp
compileExp (Array [] [x] _ _) = compileAtom x
compileExp e@(Array [_] elems (Info (TArr elem_t _)) _) = do
  elems' <- mapM compileAtom elems
  t <- compileType $ typeOf e
  elem_t' <- compileType elem_t
  F.Var <$> bind t (F.BasicOp $ F.ArrayLit elems' elem_t')
compileExp (Var v _ _) =
  let v' = F.VName (F.nameFromText (varName v)) (getTag (varTag v))
   in pure $ F.Var v'
compileExp (App (Var v _ _) [x, y] (Info (t, pframe)) _)
  | Just op <- lookup (varName v, t) binops = do
      x' <- compileExp x
      y' <- compileExp y
      t' <- compileType t
      F.Var <$> bind t' (F.BasicOp (F.BinOp op x' y'))
  where
    binops =
      [ (("+", TArr Int (Concat [])), F.Add F.Int32 F.OverflowWrap),
        (("-", TArr Int (Concat [])), F.Sub F.Int32 F.OverflowWrap)
      ]
compileExp e@(App f xs (Info (t, pframe)) _) = do
  f' <- compileFunExp f
  xs' <- traverse compileExp xs
  t' <- compileType t
  case intShape $ normShape pframe of
    [] ->
      fmap F.Var . bind t' $
        F.Apply
          f'
          (map (,F.Observe) xs')
          [(F.staticShapes1 $ flip F.toDecl F.Nonunique t', mempty)]
          F.Safe
    [d] -> F.Var <$> map1 d (typeOf f) f' (map asVar xs') t'
    _ -> error $ "compileExp: unhandled:\n" ++ show e
  where
    asVar (F.Var v) = v

    intDim :: Dim -> Int
    intDim (DimVar d) = error "intDim: AAAAAAAAAAAAAAAAAAA"
    intDim (DimN d) = d
    intDim (Add ds) = sum $ map intDim ds

    intShape :: Shape -> [Int]
    intShape (ShapeVar s) = error "intShape: AAAAAAAAAAAAAAAAAAA"
    intShape (ShapeDim d) = pure $ intDim d
    intShape (Concat ss) = concat $ map intShape ss
compileExp e = error $ "compileExp: unhandled:\n" ++ show e

valueType :: F.Type -> F.ValueType
valueType (F.Array pt shape _) =
  F.ValueType F.Signed (F.Rank (F.shapeRank shape)) pt
valueType t = error $ "valueType: unhandled " ++ show t

wrapInMain :: ((F.SubExp, F.Type), State) -> T.Text
wrapInMain ((e, ret), State stms counter funs) =
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
    initialState = State mempty 0 mempty
