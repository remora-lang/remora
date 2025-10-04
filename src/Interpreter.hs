module Interpreter where

import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Interpreter.Value hiding (Val)
import Interpreter.Value qualified as Value
import Prettyprinter
import RemoraPrelude
import Syntax hiding (Atom, Dim, Exp, Shape, Type)
import Syntax qualified
import Util
import VName

type Dim = Syntax.Dim VName

type Val = Value.Val InterpM

type Exp = Syntax.Exp Typed VName

type Atom = Syntax.Atom Typed VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

data Env = Env
  { envMap :: Map VName Val,
    envTMap :: Map VName Type
  }

initEnv :: Prelude VName InterpM -> Env
initEnv prelude = Env m tm
  where
    m =
      M.fromList $
        map (\(PreludeVal v _ val) -> (v, val)) $
          filter isVal prelude
    tm =
      M.fromList $
        map (\(PreludeVal v t _) -> (v, t)) $
          filter isVal prelude
    isVal PreludeVal {} = True
    isVal _ = False

type Error = Text

newtype InterpM a = InterpM {runInterpM :: ExceptT Error (Reader Env) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error
    )

lookupVal :: VName -> InterpM Val
lookupVal v = do
  mval <- asks ((M.!? v) . envMap)
  case mval of
    Nothing -> throwError $ "lookupVal: unknown vname " <> T.pack (show v)
    Just val -> pure val

lookupType :: VName -> InterpM Type
lookupType v = do
  mt <- asks ((M.!? v) . envTMap)
  case mt of
    Nothing -> throwError $ "lookupVal: unknown type vname " <> T.pack (show v)
    Just t -> pure t

interpret :: Prelude VName InterpM -> Exp -> Either Error Val
interpret prelude e =
  runReader (runExceptT $ runInterpM $ intExp e) (initEnv prelude)

intExp :: Exp -> InterpM Val
intExp (Var v _ _) = lookupVal v
intExp (Array shape as _ _) =
  ValArray shape <$> mapM intAtom as
intExp (EmptyArray shape _ _ _) =
  pure $ ValArray shape mempty
intExp (Frame shape es _ _) =
  ValArray shape <$> mapM intExp es
intExp (EmptyFrame shape _ _ _) =
  pure $ ValArray shape mempty
intExp (App (f : es) (Typed r) _) = do
  f' <- intExp f
  es' <- mapM intExp es
  apply f' es'
intExp (TApp e ts _ _) = do
  e' <- intExp e
  tapply e' ts
intExp (IApp e is _ _) = do
  e' <- intExp e
  is' <- mapM intShape is
  iapply e' is'
intExp (Syntax.Atom a) = intAtom a

intDim :: Dim -> InterpM Int
intDim = pure . intDim'

intDim' :: Dim -> Int
intDim' DimVar {} = error ""
intDim' (Syntax.Dim d) = d
intDim' (Add ds) = sum $ map intDim' ds

intShape :: Shape -> InterpM [Int]
intShape = pure . intShape' . normShape
  where
    intShape' ShapeVar {} = error ""
    intShape' (ShapeDim d) = [intDim' d]
    intShape' (Concat ss) =
      concatMap intShape' ss

bind :: VName -> Val -> InterpM a -> InterpM a
bind v val =
  local (\env -> env {envMap = M.insert v val $ envMap env})

binds :: [(VName, Val)] -> InterpM a -> InterpM a
binds [] m = m
binds ((v, val) : vvals) m =
  bind v val $ binds vvals m

tbind :: VName -> Type -> InterpM a -> InterpM a
tbind v t =
  local (\env -> env {envTMap = M.insert v t $ envTMap env})

tbinds :: [(VName, Type)] -> InterpM a -> InterpM a
tbinds [] m = m
tbinds ((v, t) : vts) m =
  tbind v t $ tbinds vts m

iapply :: Val -> [[Int]] -> InterpM Val
iapply (ValIFun f) shapes = f shapes
iapply (ValArray shape fs) shapes = do
  vs <- mapM (flip iapply shapes) fs
  pure $ ValArray shape vs
iapply v _ = error $ prettyString v

tapply :: Val -> [Type] -> InterpM Val
tapply (ValTLambda pts e) ts =
  tbinds (zip (map fst pts) ts) $ intExp e
tapply (ValVar f) ts = do
  f' <- lookupVal f
  case f' of
    ValTLambda {} -> tapply f' ts
    ValTFun func -> func ts
    _ -> throwError $ "cannot apply: " <> prettyText f'
tapply (ValTFun f) ts = f ts
tapply (ValArray shape fs) ts = do
  vs <- mapM (flip tapply ts) fs
  pure $ ValArray shape vs
tapply v _ = error $ prettyString v

apply :: Val -> [Val] -> InterpM Val
apply (ValLambda pts e) args =
  binds (zip (map fst pts) args) $
    intExp e
apply (ValVar f) args = do
  f' <- lookupVal f
  case f' of
    ValLambda {} -> apply f' args
    ValFun func -> func args
    _ -> throwError $ "cannot apply: " <> prettyText f'
apply (ValArray shape fs) args = do
  vs <- mapM (flip apply args) fs
  pure $ ValArray shape vs
apply (ValFun f) args = f args

intAtom :: Atom -> InterpM Val
intAtom (Base b _ _) = pure $ ValBase b
intAtom (Lambda ps e _ _) = pure $ ValLambda ps e
intAtom (TLambda ps e _ _) = pure $ ValTLambda ps e
intAtom (ILambda ps e _ _) = pure $ ValILambda ps e
intAtom (Box is e t _) =
  ValBox is <$> intExp e <*> pure t -- fix
