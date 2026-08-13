module LambdaLift.Monad where

import Binds
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Free
import Prop
import Syntax hiding (ArrayType, ISpaceParam, TypeParam, bindVars)
import Syntax qualified
import Util (prettyString)
import VName

type ArrayType = Syntax.ArrayType VName

type ISpaceParam = Syntax.ISpaceParam VName

type TypeParam = Syntax.TypeParam VName

data Env = Env
  { envVars :: Map VName ArrayType,
    envTypeVars :: Set VName,
    envISpaceVars :: Map VName ISpaceParam
  }

initEnv :: Env
initEnv = Env mempty mempty mempty

data St = St
  { stateBinds :: [Bind],
    stateLifted :: Map VName Exp,
    stateTag :: Tag
  }

initSt :: Tag -> St
initSt = St mempty mempty

instance HasBinds St where
  getBinds = stateBinds
  putBinds bs st = st {stateBinds = bs}

newtype LiftM a = LiftM {runLiftM :: ReaderT Env (State St) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState St
    )

runLift :: Tag -> LiftM a -> (a, Tag)
runLift tag =
  second stateTag
    . flip runState (initSt tag)
    . flip runReaderT initEnv
    . runLiftM

instance MonadVName LiftM where
  getVarTag = gets stateTag
  putVarTag tag = modify $ \st -> st {stateTag = tag}

lookupVar :: VName -> LiftM (Maybe ArrayType)
lookupVar v = asks (M.lookup v . envVars)

addLifted :: VName -> Exp -> LiftM ()
addLifted v e = modify $ \st -> st {stateLifted = M.insert v e $ stateLifted st}

lookupLifted :: VName -> LiftM (Maybe Exp)
lookupLifted v = gets $ M.lookup v . stateLifted

bindVar :: VName -> ArrayType -> LiftM a -> LiftM a
bindVar v t = bindVars [(v, t)]

bindVars :: [(VName, ArrayType)] -> LiftM a -> LiftM a
bindVars vts = local $
  \env -> env {envVars = M.fromList vts <> envVars env}

bindTypeParams :: (Foldable t) => t TypeParam -> LiftM a -> LiftM a
bindTypeParams ps = local $
  \env ->
    env
      { envTypeVars =
          S.fromList (map unTypeParam $ toList ps) <> envTypeVars env
      }

lookupISpaceParam :: VName -> LiftM (Maybe ISpaceParam)
lookupISpaceParam v = asks $ M.lookup v . envISpaceVars

bindISpaceParams :: (Foldable t) => t ISpaceParam -> LiftM a -> LiftM a
bindISpaceParams ps = local $
  \env ->
    env
      { envISpaceVars =
          M.fromList (map (\p -> (unISpaceParam p, p)) $ toList ps)
            <> envISpaceVars env
      }

data Captured = Captured
  { capturedTerms :: [(VName, ArrayType)],
    capturedTypes :: [VName],
    capturedISpaces :: [ISpaceParam]
  }

capturedParams :: Captured -> [Pat]
capturedParams = map (uncurry mkParam) . capturedTerms

appCaptured :: VName -> Captured -> ArrayType -> Exp
appCaptured v capt t =
  flip mkApp (map (uncurry mkVar) $ capturedTerms capt) $
    foldl mkISpaceApp (foldl mkTypeApp (mkVar v t) $ capturedTypes capt) $
      capturedISpaces capt

captured :: (Free VName a) => a -> LiftM Captured
captured x =
  Captured
    <$> (catMaybes <$> traverse capturedTerm (S.toList $ freeTermVars free))
    <*> traverse capturedType (S.toList $ freeTypeVars free)
    <*> traverse capturedISpace (S.toList $ freeISpaceVars free)
  where
    free = freeVars x

    capturedTerm v = (fmap . fmap) (v,) $ lookupVar v

    capturedType v = do
      inscope <- asks $ S.member v . envTypeVars
      unless inscope $ notInScope "type" v
      pure v

    capturedISpace v =
      maybe (notInScope "index" v) pure =<< lookupISpaceParam v

    notInScope kind v =
      error $
        "captured: " <> kind <> " var not in scope: " <> prettyString v

withBind :: Bind -> LiftM a -> LiftM a
withBind (BindVal v _ e _) = bindVar v $ arrayTypeOf e
withBind (BindFun f _ _ _ (Info ft) _) = bindVar f $ mkScalarArrayType ft
withBind (BindTFun f _ _ _ (Info ft) _) = bindVar f $ mkScalarArrayType ft
withBind (BindIFun f _ _ _ (Info ft) _) = bindVar f $ mkScalarArrayType ft
withBind (BindType tp _ _ _) = bindTypeParams [tp]
withBind (BindISpace ip _ _) = bindISpaceParams [ip]

withBinds :: [Bind] -> LiftM a -> LiftM a
withBinds bs m = foldr withBind m bs
