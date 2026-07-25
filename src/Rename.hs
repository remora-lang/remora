module Rename (rename, renameExp) where

import Control.Monad.Reader
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import ISpace
import Pass (PassM)
import Substitute
import Syntax
import VName

data Env = Env
  { envMap :: Map VName VName,
    envSubst :: Subst VName
  }

initEnv :: Env
initEnv = Env mempty mempty

newtype RenameM m a = RenameM {runRenameM :: ReaderT Env m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env
    )

instance MonadTrans RenameM where
  lift = RenameM . lift

instance (MonadVName m) => MonadVName (RenameM m) where
  getVarTag = lift getVarTag
  putVarTag = lift . putVarTag

runRename :: RenameM m a -> m a
runRename = flip runReaderT initEnv . runRenameM

rename :: Prog -> PassM Prog
rename = runRename . renameProg

renameExp :: (MonadVName m) => Exp -> m Exp
renameExp = runRename . renameExp'

useVar :: (Monad m) => VName -> RenameM m VName
useVar v = asks $ fromMaybe v . M.lookup v . envMap

sub :: (Monad m, Substitutable VName a) => a -> RenameM m a
sub x = asks $ \env -> substitute (envSubst env) x

withParams :: (MonadVName m) => [VName] -> RenameM m a -> RenameM m a
withParams vs m = do
  vs' <- mapM (newVName . varName) vs
  local (extend $ zip vs vs') m
  where
    extend pairs (Env mp subst) =
      Env
        (M.fromList pairs <> mp)
        (foldMap (uncurry renameVar) pairs <> subst)

renameProg :: (MonadVName m) => Prog -> RenameM m Prog
renameProg (Prog decs) =
  withParams (concatMap declBinders decs) $
    Prog <$> traverse renameDecl decs

renameDecl :: (MonadVName m) => Decl -> RenameM m Decl
renameDecl (Def b) = Def <$> renameBind b
renameDecl (Entry v pats mte body t pos) = do
  v' <- useVar v
  mte' <- traverse sub mte
  t' <- sub t
  withParams (patVar <$> pats) $
    Entry v'
      <$> traverse renamePat pats
      <*> pure mte'
      <*> renameExp' body
      <*> pure t'
      <*> pure pos

renameExp' :: (MonadVName m) => Exp -> RenameM m Exp
renameExp' (Var x t pos) =
  Var <$> useVar x <*> sub t <*> pure pos
renameExp' (Array s as t pos) =
  Array s <$> traverse renameAtom as <*> sub t <*> pure pos
renameExp' (EmptyArray s te t pos) =
  EmptyArray s <$> sub te <*> sub t <*> pure pos
renameExp' (Frame s es t pos) =
  Frame s <$> traverse renameExp' es <*> sub t <*> pure pos
renameExp' (EmptyFrame s te t pos) =
  EmptyFrame s <$> sub te <*> sub t <*> pure pos
renameExp' (App f arg t pos) =
  App <$> renameExp' f <*> renameExp' arg <*> subPair t <*> pure pos
renameExp' (TApp e te t pos) =
  TApp <$> renameExp' e <*> sub te <*> sub t <*> pure pos
renameExp' (IApp e isp t pos) =
  IApp <$> renameExp' e <*> sub isp <*> sub t <*> pure pos
renameExp' (Unbox ip x box body t pos) = do
  box' <- renameExp' box
  t' <- sub t
  withParams [unISpaceParam ip, x] $
    Unbox
      <$> renameISpaceParam ip
      <*> useVar x
      <*> pure box'
      <*> renameExp' body
      <*> pure t'
      <*> pure pos
renameExp' (Let bs body t pos) = do
  t' <- sub t
  withParams (binderVar <$> NE.toList bs) $
    Let
      <$> traverse renameBind bs
      <*> renameExp' body
      <*> pure t'
      <*> pure pos

subPair ::
  (Monad m) =>
  Info (ArrayType VName, Shape VName) ->
  RenameM m (Info (ArrayType VName, Shape VName))
subPair (Info (at, sh)) = do
  at' <- sub at
  sh' <- sub sh
  pure $ Info (at', sh')

renameAtom :: (MonadVName m) => Atom -> RenameM m Atom
renameAtom (Base b t pos) =
  Base b <$> sub t <*> pure pos
renameAtom (Lambda pat body t pos) = do
  t' <- sub t
  withParams [patVar pat] $
    Lambda <$> renamePat pat <*> renameExp' body <*> pure t' <*> pure pos
renameAtom (TLambda tp body t pos) = do
  t' <- sub t
  withParams [unTypeParam tp] $
    TLambda <$> renameTypeParam tp <*> renameExp' body <*> pure t' <*> pure pos
renameAtom (ILambda ip body t pos) = do
  t' <- sub t
  withParams [unISpaceParam ip] $
    ILambda <$> renameISpaceParam ip <*> renameExp' body <*> pure t' <*> pure pos
renameAtom (Box isp body te t pos) =
  Box <$> sub isp <*> renameExp' body <*> sub te <*> sub t <*> pure pos

renameBind :: (MonadVName m) => Bind -> RenameM m Bind
renameBind (BindVal v mte e pos) =
  BindVal <$> useVar v <*> traverse sub mte <*> renameExp' e <*> pure pos
renameBind (BindFun v pats mte body t pos) = do
  v' <- useVar v
  mte' <- traverse sub mte
  t' <- sub t
  withParams (patVar <$> NE.toList pats) $
    BindFun v'
      <$> traverse renamePat pats
      <*> pure mte'
      <*> renameExp' body
      <*> pure t'
      <*> pure pos
renameBind (BindTFun v tps mte body t pos) = do
  v' <- useVar v
  t' <- sub t
  withParams (unTypeParam <$> NE.toList tps) $
    BindTFun v'
      <$> traverse renameTypeParam tps
      <*> traverse sub mte
      <*> renameExp' body
      <*> pure t'
      <*> pure pos
renameBind (BindIFun v ips mte body t pos) = do
  v' <- useVar v
  t' <- sub t
  withParams (unISpaceParam <$> NE.toList ips) $
    BindIFun v'
      <$> traverse renameISpaceParam ips
      <*> traverse sub mte
      <*> renameExp' body
      <*> pure t'
      <*> pure pos
renameBind (BindType tp te t pos) =
  BindType <$> renameTypeParam tp <*> sub te <*> sub t <*> pure pos
renameBind (BindISpace ip isp pos) =
  BindISpace <$> renameISpaceParam ip <*> sub isp <*> pure pos

renamePat :: (Monad m) => Pat -> RenameM m Pat
renamePat (PatId v te t pos) =
  PatId <$> useVar v <*> sub te <*> sub t <*> pure pos

renameTypeParam :: (Monad m) => TypeParam VName -> RenameM m (TypeParam VName)
renameTypeParam tp = AtomTypeParam <$> useVar (unTypeParam tp)

renameISpaceParam :: (Monad m) => ISpaceParam VName -> RenameM m (ISpaceParam VName)
renameISpaceParam ip = (<$ ip) <$> useVar (unISpaceParam ip)

declBinders :: Decl -> [VName]
declBinders (Def b) = [binderVar b]
declBinders (Entry v _ _ _ _ _) = [v]

binderVar :: Bind -> VName
binderVar (BindVal v _ _ _) = v
binderVar (BindFun v _ _ _ _ _) = v
binderVar (BindTFun v _ _ _ _ _) = v
binderVar (BindIFun v _ _ _ _ _) = v
binderVar (BindType tp _ _ _) = unTypeParam tp
binderVar (BindISpace ip _ _) = unISpaceParam ip
