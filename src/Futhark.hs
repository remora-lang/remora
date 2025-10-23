module Futhark (FutharkM, compile) where

import Control.Monad.Error.Class
import Control.Monad.Reader hiding (lift)
import Control.Monad.Trans.Except
import Data.List qualified as L
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

data Env = Env

type Error = T.Text

-- | The Futhark generator monad.
newtype FutharkM a = FutharkM {runFutharkM :: ExceptT Error (Reader Env) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error
    )

type FutharkExp = Doc ()

type FutharkParam = Doc ()

type FutharkType = Doc ()

type FutharkShape = Doc ()

type FutharkDim = Doc ()

apply :: FutharkExp -> FutharkExp -> FutharkExp
apply f x = parens f <+> parens x

compileDim :: Dim -> FutharkM FutharkDim
compileDim (DimN x) = pure $ pretty x
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
  pure $ parens (pretty v <> ":" <+> t')

compileAtom :: Atom -> FutharkM FutharkExp
compileAtom (Base (BoolVal True) _ _) = pure "true"
compileAtom (Base (BoolVal False) _ _) = pure "false"
compileAtom (Base (IntVal x) _ _) = pure $ pretty x <> "i32"
compileAtom (Base (FloatVal x) _ _) = pure $ pretty x <> "f64"
compileAtom (Lambda params body _ _) = do
  params' <- mapM compileParam params
  body' <- compileExp body
  pure $
    vcat
      [ "\\" <> mconcat params' <+> "->",
        indent 2 body'
      ]
compileAtom e = error $ "compileAtom: unhandled:\n" ++ show e

compileExp :: Exp -> FutharkM FutharkExp
compileExp (Array [] [x] _ _) = compileAtom x
compileExp (Array [_] elems _ _) = do
  elems' <- mapM compileAtom elems
  pure $ "[" <> mconcat (punctuate "," elems') <> "]"
compileExp (Var v _ _)
  | varName v `elem` operators = pure $ pretty $ varName v
  | otherwise = pure $ parens $ pretty v
  where
    operators = ["+", "-"]
compileExp (App f xs _ _) = do
  f' <- compileExp f
  xs' <- traverse compileExp xs
  pure $ L.foldl' apply f' xs'
compileExp e = error $ "compileExp: unhandled:\n" ++ show e

wrapInMain :: FutharkExp -> T.Text
wrapInMain e =
  renderStrict . layoutPretty defaultLayoutOptions $
    vcat ["entry main () =", indent 2 e]

-- | Turn Remora into Futhark.
compile :: Prelude VName FutharkM -> Exp -> Either Error T.Text
compile _prelude e =
  wrapInMain <$> runReader (runExceptT (runFutharkM (compileExp e))) Env
