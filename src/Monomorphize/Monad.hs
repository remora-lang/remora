module Monomorphize.Monad where

import Binds
import Control.Monad (unless)
import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Free
import Syntax hiding (ArrayType, AtomType, ISpace, ISpaceParam, TypeParam, bindVars)
import Syntax qualified
import Util (Error, prettyString)
import VName

type ISpace = Syntax.ISpace VName

type TypeParam = Syntax.TypeParam VName

type ISpaceParam = Syntax.ISpaceParam VName

type AtomType = Syntax.AtomType VName

type ArrayType = Syntax.ArrayType VName

data Param
  = ParamType TypeParam
  | ParamISpace ISpaceParam
  deriving (Eq, Ord, Show)

data Arg
  = ArgType AtomType
  | ArgISpace ISpace
  deriving (Eq, Ord, Show)

-- | A polymorphic value. There are two kinds: 1) A 'PolyFun' is a polymorphic
-- function: an optional 'VName' (for named functions), the list of polymorphic
-- quantifier params it still expects, and a body that is free in them. 2) A
-- 'PolyArray' is an array of 'Poly'.
data Poly
  = PolyFun (Maybe VName) [Param] Exp
  | PolyArray [Int] (NonEmpty Poly)
  deriving (Show)

data Env = Env
  { -- | The polymorphic definitions in scope.
    envDefs :: Map VName Poly,
    -- | Types of the value variables in scope.
    envVars :: Map VName ArrayType,
    -- | ISpace vars in scope.
    envISpaceParams :: Map VName ISpaceParam
  }

initEnv :: Env
initEnv = Env mempty mempty mempty

data St = St
  { -- | Existant monomorphic instances (keyed by the name and arguments).
    stateMonoMap :: Map (VName, [Arg]) Exp,
    stateBinds :: [Bind],
    stateTag :: Tag
  }

initSt :: Tag -> St
initSt = St mempty mempty

instance HasBinds St where
  getBinds = stateBinds
  putBinds bs st = st {stateBinds = bs}

newtype MonoM a = MonoM {runMonoM :: ReaderT Env (ExceptT Error (State St)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState St,
      MonadError Error
    )

runMono :: Tag -> MonoM a -> (Either Error a, Tag)
runMono tag =
  second stateTag
    . flip runState (initSt tag)
    . runExceptT
    . flip runReaderT initEnv
    . runMonoM

instance MonadVName MonoM where
  getVarTag = gets stateTag
  putVarTag tag = modify $ \st -> st {stateTag = tag}

bindDef :: VName -> Poly -> MonoM a -> MonoM a
bindDef v d = local $
  \env -> env {envDefs = M.insert v d $ envDefs env}

lookupDef :: VName -> MonoM (Maybe Poly)
lookupDef v = asks (M.lookup v . envDefs)

bindVar :: VName -> ArrayType -> MonoM a -> MonoM a
bindVar v t = bindVars [(v, t)]

bindVars :: [(VName, ArrayType)] -> MonoM a -> MonoM a
bindVars vts = local $
  \env -> env {envVars = M.fromList vts <> envVars env}

lookupVar :: VName -> MonoM (Maybe ArrayType)
lookupVar v = asks (M.lookup v . envVars)

bindISpaceParams :: (Foldable t) => t ISpaceParam -> MonoM a -> MonoM a
bindISpaceParams ps = local $
  \env ->
    env
      { envISpaceParams =
          M.fromList (map (\p -> (unISpaceParam p, p)) $ toList ps)
            <> envISpaceParams env
      }

lookupISpaceParam :: VName -> MonoM (Maybe ISpaceParam)
lookupISpaceParam v = asks (M.lookup v . envISpaceParams)

lookupMono :: VName -> [Arg] -> MonoM (Maybe Exp)
lookupMono v args =
  (M.!? (v, args)) <$> gets stateMonoMap

emitMonoExp :: (VName, [Arg]) -> Exp -> MonoM ()
emitMonoExp k e =
  modify $ \st -> st {stateMonoMap = M.insert k e $ stateMonoMap st}

captured :: Exp -> MonoM ([(VName, ArrayType)], [ISpaceParam])
captured body = do
  unless (S.null $ freeTypeVars free) $
    error $
      "captured: instance is free in type vars: "
        <> unwords (map prettyString $ S.toList $ freeTypeVars free)
  (,)
    <$> (catMaybes <$> traverse capturedTerm (S.toList $ freeTermVars free))
    <*> traverse capturedISpace (S.toList $ freeISpaceVars free)
  where
    free = freeVars body

    capturedTerm v = fmap (v,) <$> lookupVar v

    capturedISpace v =
      maybe (notInScope v) pure =<< lookupISpaceParam v

    notInScope v =
      error $ "captured: index var not in scope: " <> prettyString v
