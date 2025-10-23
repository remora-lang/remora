module Futhark (FutharkM, compile) where

import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.State hiding (State)
import Control.Monad.Trans.Except
import Data.Text qualified as T
import Prettyprinter
import Prettyprinter.Render.Text
import RemoraPrelude (Prelude)
import Syntax hiding (Atom, Dim, Exp, Idx, Shape, Type, (\\))
import Syntax qualified
import VName

type Dim = Syntax.Dim VName

type Exp = Syntax.Exp Typed VName

type Atom = Syntax.Atom Typed VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

type FutharkExp = Doc ()

type FutharkParam = Doc ()

type FutharkType = Doc ()

type FutharkShape = Doc ()

type FutharkDim = Doc ()

type FutharkStm = Doc ()

type FutharkStms = Doc ()

type FutharkVar = Doc ()

type FutharkFun = Doc ()

type FutharkBody = Doc ()

data Env = Env

type Error = T.Text

data State = State
  { stateStms :: FutharkStms,
    stateCounter :: Int,
    stateFuns :: [FutharkFun]
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

emit :: FutharkStm -> FutharkM ()
emit stm = modify $ \s -> s {stateStms = vsep [stateStms s, stm]}

collect :: FutharkM a -> FutharkM (a, FutharkStms)
collect m = do
  old_stms <- gets stateStms
  x <- m
  stms <- gets stateStms
  modify $ \s -> s {stateStms = old_stms}
  pure (x, stms)

newVar :: FutharkM FutharkVar
newVar = do
  x <- gets stateCounter
  modify $ \s -> s {stateCounter = succ x}
  pure $ "v_" <> pretty x

bind :: FutharkType -> FutharkExp -> FutharkM FutharkVar
bind t e = do
  v <- newVar
  emit $ "let" <+> braces (v <> ":" <+> t) <+> "=" <+> e
  pure v

mkBody :: FutharkM [FutharkVar] -> FutharkM FutharkBody
mkBody m = do
  (vs, stms) <- collect m
  pure $ vsep [stms, "in" <+> braces (mconcat $ punctuate ", " vs)]

addFunction :: FutharkFun -> FutharkM ()
addFunction fun = modify $ \s -> s {stateFuns = stateFuns s ++ [fun]}

findRet :: Type -> Type
findRet (_ :-> t) = findRet t
findRet t = t

-- Assumes the lambda has no free variables.
liftLambda :: [(VName, Type)] -> Exp -> Type -> FutharkM FutharkVar
liftLambda params body ret = do
  params' <- mapM compileParam params
  body' <- mkBody $ pure <$> compileExp body
  ret' <- compileType $ findRet ret
  fname <- newVar
  addFunction $
    vsep
      [ "fun" <+> fname <+> parens (mconcat $ punctuate ", " params'),
        "  :" <+> braces ret' <+> "= {",
        indent 2 body',
        "}"
      ]
  pure fname

compileDim :: Dim -> FutharkM FutharkDim
compileDim (DimN x) = pure $ pretty x <> "i64"
compileDim d = error $ "compileDim: unhandled:\n" ++ show d

compileShape :: Shape -> FutharkM FutharkShape
compileShape (ShapeDim d) = brackets <$> compileDim d
compileShape (Concat ds) = mconcat <$> mapM compileShape ds
compileShape s = error $ "compileShape: unhandled:\n" ++ show s

compileType :: Type -> FutharkM FutharkType
compileType Bool = pure "bool"
compileType Int = pure "i32"
compileType Float = pure "f64"
compileType (TArr t shape) = do
  t' <- compileType t
  shape' <- compileShape shape
  pure $ shape' <> t'
compileType t = error $ "compileType: unhandled:\n" ++ show t

compileParam :: (VName, Type) -> FutharkM FutharkParam
compileParam (v, t) = do
  t' <- compileType t
  pure $ pretty v <> ":" <+> t'

compileAtom :: Atom -> FutharkM FutharkExp
compileAtom (Base (BoolVal True) _ _) = pure "true"
compileAtom (Base (BoolVal False) _ _) = pure "false"
compileAtom (Base (IntVal x) _ _) = pure $ pretty x <> "i32"
compileAtom (Base (FloatVal x) _ _) = pure $ pretty x <> "f64"
compileAtom (Lambda params body (Typed t) _) = do
  liftLambda params body t
compileAtom e = error $ "compileAtom: unhandled:\n" ++ show e

compileExp :: Exp -> FutharkM FutharkVar
compileExp (Array [] [x] _ _) = compileAtom x
compileExp (Array [_] elems _ _) = do
  elems' <- mapM compileAtom elems
  pure $ "[" <> mconcat (punctuate "," elems') <> "]"
compileExp (Var v _ _)
  | varName v `elem` operators = pure $ pretty $ varName v
  | otherwise = pure $ parens $ pretty v
  where
    operators = ["+", "-"]
compileExp (App (Var v _ _) [x, y] (Typed (t, _)) _)
  | Just v' <- lookup (varName v, t) binops = do
      x' <- compileExp x
      y' <- compileExp y
      t' <- compileType t
      bind t' $ v' <> parens (x' <> "," <> y')
  where
    binops =
      [ (("+", TArr Int (Concat [])), "add32"),
        (("-", TArr Int (Concat [])), "sub32")
      ]
compileExp (App f xs (Typed (t, _)) _) = do
  f' <- compileExp f
  xs' <- traverse compileExp xs
  t' <- compileType t
  bind t' $ "apply" <+> f' <+> hsep xs'
compileExp e = error $ "compileExp: unhandled:\n" ++ show e

wrapInMain :: (FutharkVar, State) -> T.Text
wrapInMain (e, State stms _ funs) =
  renderStrict . layoutPretty defaultLayoutOptions $
    vcat $
      funs
        ++ [ "entry (\"main\", {}, {i32}) entry_main () : {i32} = {",
             indent 2 (vcat [stms, "in" <+> braces e]),
             "}"
           ]

-- | Turn Remora into Futhark.
compile :: Prelude VName FutharkM -> Exp -> Either Error T.Text
compile _prelude e =
  wrapInMain
    <$> runExcept
      ( runReaderT
          (runStateT (runFutharkM (compileExp e)) initialState)
          Env
      )
  where
    initialState = State mempty 0 mempty
