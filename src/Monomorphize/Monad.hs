module Monomorphize.Monad where

import Binds
import Control.Monad.Error.Class
import Control.Monad.State
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as M
import Syntax hiding (ArrayType, AtomType, ISpace, ISpaceParam, Type, TypeExp, TypeParam)
import Syntax qualified
import Util (Error)
import VName

type ISpace = Syntax.ISpace VName

type TypeExp = Syntax.TypeExp VName

type Type = Syntax.Type VName

type TypeParam = Syntax.TypeParam VName

type ISpaceParam = Syntax.ISpaceParam VName

type AtomType = Syntax.AtomType VName

type ArrayType = Syntax.ArrayType VName

type MonoArg = Either AtomType ISpace

type Param = Either TypeParam ISpaceParam

type Arg = Either AtomType ISpace

-- | A polymorphic value. There are two kinds: 1) A 'PolyFun' is a polymorphic
-- function: an optional 'VName' (for named functions), the list of polymorphic
-- quantifier params it still expects, and a body that is free in them. 2) A
-- 'PolyArray' is an array of 'Poly'.
data Poly
  = PolyFun (Maybe VName) [Param] Exp
  | PolyArray [Int] (NonEmpty Poly)
  deriving (Show)

data St = St
  { stateDefs :: Map VName Poly,
    stateMonoMap :: Map (VName, [MonoArg]) VName,
    stateMonoBinds :: [Bind],
    stateTag :: Tag
  }

initSt :: Tag -> St
initSt = St mempty mempty mempty

instance HasBinds St where
  getBinds = stateMonoBinds
  putBinds bs st = st {stateMonoBinds = bs}

newtype MonoM a = MonoM {runMonoM :: ExceptT Error (State St) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState St,
      MonadError Error
    )

runMono :: Tag -> MonoM a -> (Either Error a, Tag)
runMono tag =
  second stateTag
    . flip runState (initSt tag)
    . runExceptT
    . runMonoM

instance MonadVName MonoM where
  getVarTag = gets stateTag
  putVarTag tag = modify $ \st -> st {stateTag = tag}

insertDef :: VName -> Poly -> MonoM ()
insertDef v d =
  modify $ \st -> st {stateDefs = M.insert v d $ stateDefs st}

lookupDef :: VName -> MonoM (Maybe Poly)
lookupDef v =
  (M.!? v) <$> gets stateDefs

lookupMono :: VName -> [MonoArg] -> MonoM (Maybe VName)
lookupMono v args =
  (M.!? (v, args)) <$> gets stateMonoMap

emitMonoVName :: (VName, [MonoArg]) -> VName -> MonoM ()
emitMonoVName k v =
  modify $ \st -> st {stateMonoMap = M.insert k v $ stateMonoMap st}
