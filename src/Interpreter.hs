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
  { envMap :: Map VName Val
  }

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

instance Show Val

instance Pretty Val

lookupVal :: VName -> InterpM Val
lookupVal v = do
  mval <- asks ((M.!? v) . envMap)
  case mval of
    Nothing -> throwError $ "lookupVal: unknown vname " <> T.pack (show v)
    Just val -> pure val

int :: Exp -> InterpM Val
int (Var v _ _) = lookupVal v
int (Array shape as _ _) =
  ValArray shape <$> mapM intAtom as
int (EmptyArray shape _ _ _) =
  pure $ ValArray shape mempty
int (Frame shape es _ _) =
  ValArray shape <$> mapM int es
int (EmptyFrame shape _ _ _) =
  pure $ ValArray shape mempty
int (App (f : es) _ _) = do
  f' <- int f
  es' <- mapM int es
  apply f' es'

bind :: VName -> Val -> InterpM a -> InterpM a
bind v val =
  local (\env -> env {envMap = M.insert v val $ envMap env})

binds :: [(VName, Val)] -> InterpM a -> InterpM a
binds [] m = m
binds ((v, val) : vvals) m =
  bind v val $ binds vvals m

apply :: Val -> [Val] -> InterpM Val
apply (ValLambda pts e) args =
  binds (zip (map fst pts) args) $
    int e
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
  ValBox is <$> int e <*> pure t -- fix

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
