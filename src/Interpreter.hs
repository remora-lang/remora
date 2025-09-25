module Interpreter where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Syntax hiding (Atom, Exp, Idx, Type)
import Syntax qualified
import Util
import VName

type Exp = Syntax.Exp Typed VName

type Atom = Syntax.Atom Typed VName

type Idx = Syntax.Idx VName

type Type = Syntax.Type VName

data Env = Env
  { envMap :: Map VName Val,
    envTMap :: Map VName Type
  }

initEnv :: Env
initEnv = Env mempty mempty

type Error = Text

newtype InterpM a = InterpM {runInterpM :: ExceptT Error (Reader Env) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Error
    )

data Val
  = ValVar VName
  | ValBase Base
  | ValArray [Int] [Val]
  | ValLambda [(VName, Type)] Exp
  | ValTLambda [(VName, Kind)] Exp
  | ValILambda [(VName, Sort)] Exp
  | ValBox [Idx] Val Type
  | ValFun ([Val] -> InterpM Val)
  | ValTFun ([Type] -> InterpM Val)

instance Show Val

instance Pretty Val

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

interpret :: Exp -> Either Error Val
interpret e =
  runReader (runExceptT $ runInterpM $ intExp e) initEnv

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
intExp (App (f : es) _ _) = do
  f' <- intExp f
  es' <- mapM intExp es
  apply f' es'
intExp (TApp e ts _ _) = do
  e' <- intExp e
  undefined

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

tapply :: Val -> [Type] -> InterpM Val
tapply (ValTLambda pts e) ts =
  tbinds (zip (map fst pts) ts) $ intExp e
tapply (ValVar f) ts = do
  f' <- lookupVal f
  case f' of
    ValTLambda {} -> tapply f' ts
    ValTFun func -> func ts
    _ -> throwError $ "cannot apply: " <> prettyText f'

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

intAtom :: Atom -> InterpM Val
intAtom (Base b _ _) = pure $ ValBase b
intAtom (Lambda ps e _ _) = pure $ ValLambda ps e
intAtom (TLambda ps e _ _) = pure $ ValTLambda ps e
intAtom (ILambda ps e _ _) = pure $ ValILambda ps e
intAtom (Box is e t _) =
  ValBox is <$> intExp e <*> pure t -- fix

prelude :: [(Text, Syntax.Type Text, Val)]
prelude =
  [ ( "head",
      Forall
        [("t", KindAtom)]
        ( DProd
            [("d", SortDim)]
            ( DProd
                [("s", SortShape)]
                ( [TArr (TVar "t") (Shape [Add [Dim 1, IdxVar "d"], IdxVar "s"])]
                    :-> TArr (TVar "t") (IdxVar "s")
                )
            )
        ),
      ValFun $ \xs ->
        case xs of
          [ValArray shape (_ : vs)] ->
            -- TODO: shape must be decremented
            pure $ ValArray shape vs
          _ -> error $ "head: " <> show xs
    )
  ]
