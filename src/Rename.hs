{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Rename (renameExp, Renameable (..), MonadRename (..)) where

import Control.Monad.Reader
import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import ISpace
import Substitute
import Syntax
import VName

newtype RenameM m a = RenameM {runRenameM :: ReaderT (Subst VName) m a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader (Subst VName)
    )

instance MonadTrans RenameM where
  lift = RenameM . lift

instance (MonadVName m) => MonadVName (RenameM m) where
  getVarTag = lift getVarTag
  putVarTag = lift . putVarTag

class (MonadVName m) => MonadRename m where
  applyRename :: (Substitutable VName a) => a -> m a
  withRenamedBinders :: [VName] -> m a -> m a

instance (MonadVName m) => MonadRename (RenameM m) where
  applyRename x = asks $ flip substitute x
  withRenamedBinders vs m = do
    pairs <- zip vs <$> mapM (newVName . varName) vs
    local (foldMap (uncurry renameVar) pairs <>) m

renameExp :: (MonadVName m) => Exp -> m Exp
renameExp = flip runReaderT mempty . runRenameM . rename

class Renameable a where
  rename :: (MonadRename m) => a -> m a

instance
  ( Traversable tp,
    Substitutable VName (te VName),
    forall a. (Substitutable VName a) => Substitutable VName (f a)
  ) =>
  Renameable (ExpBase te tp f VName)
  where
  rename (Var x t pos) =
    Var <$> applyRename x <*> applyRename t <*> pure pos
  rename (Array s as t pos) =
    Array s <$> traverse rename as <*> applyRename t <*> pure pos
  rename (EmptyArray s te t pos) =
    EmptyArray s <$> applyRename te <*> applyRename t <*> pure pos
  rename (Frame s es t pos) =
    Frame s <$> traverse rename es <*> applyRename t <*> pure pos
  rename (EmptyFrame s te t pos) =
    EmptyFrame s <$> applyRename te <*> applyRename t <*> pure pos
  rename (App f arg t pos) =
    App <$> rename f <*> rename arg <*> applyRename t <*> pure pos
  rename (TApp e te t pos) =
    TApp <$> rename e <*> applyRename te <*> applyRename t <*> pure pos
  rename (IApp e isp t pos) =
    IApp <$> rename e <*> applyRename isp <*> applyRename t <*> pure pos
  rename (Unbox ip x box body t pos) =
    withRenamedBinders (unISpaceParam ip : [x]) $
      Unbox
        <$> traverse applyRename ip
        <*> applyRename x
        <*> rename box
        <*> rename body
        <*> applyRename t
        <*> pure pos
  rename (Let bs body t pos) =
    withRenamedBinders (concatMap bindVars $ NE.toList bs) $
      Let <$> traverse rename bs <*> rename body <*> applyRename t <*> pure pos

instance
  ( Traversable tp,
    Substitutable VName (te VName),
    forall a. (Substitutable VName a) => Substitutable VName (f a)
  ) =>
  Renameable (AtomBase te tp f VName)
  where
  rename (Base b t pos) =
    Base b <$> applyRename t <*> pure pos
  rename (Lambda pat body t pos) =
    withRenamedBinders [patVar pat] $
      Lambda <$> rename pat <*> rename body <*> applyRename t <*> pure pos
  rename (TLambda tp body t pos) =
    withRenamedBinders (toList tp) $
      TLambda <$> traverse applyRename tp <*> rename body <*> applyRename t <*> pure pos
  rename (ILambda ip body t pos) =
    withRenamedBinders (toList ip) $
      ILambda <$> traverse applyRename ip <*> rename body <*> applyRename t <*> pure pos
  rename (Box isp body te t pos) =
    Box <$> applyRename isp <*> rename body <*> applyRename te <*> applyRename t <*> pure pos

instance
  ( Traversable tp,
    Substitutable VName (te VName),
    forall a. (Substitutable VName a) => Substitutable VName (f a)
  ) =>
  Renameable (BindBase te tp f VName)
  where
  rename (BindVal v mte e pos) =
    BindVal
      <$> applyRename v
      <*> traverse applyRename mte
      <*> rename e
      <*> pure pos
  rename (BindFun v pats mte body t pos) =
    withRenamedBinders (patVar <$> NE.toList pats) $
      BindFun
        <$> applyRename v
        <*> traverse rename pats
        <*> traverse applyRename mte
        <*> rename body
        <*> applyRename t
        <*> pure pos
  rename (BindTFun v tps mte body t pos) =
    withRenamedBinders (concatMap toList tps) $
      BindTFun
        <$> applyRename v
        <*> traverse (traverse applyRename) tps
        <*> traverse applyRename mte
        <*> rename body
        <*> applyRename t
        <*> pure pos
  rename (BindIFun v ips mte body t pos) =
    withRenamedBinders (concatMap toList ips) $
      BindIFun
        <$> applyRename v
        <*> traverse (traverse applyRename) ips
        <*> traverse applyRename mte
        <*> rename body
        <*> applyRename t
        <*> pure pos
  rename (BindType tp te t pos) =
    BindType <$> traverse applyRename tp <*> applyRename te <*> applyRename t <*> pure pos
  rename (BindISpace ip isp pos) =
    BindISpace <$> traverse applyRename ip <*> applyRename isp <*> pure pos

instance
  ( Substitutable VName (te VName),
    forall a. (Substitutable VName a) => Substitutable VName (f a)
  ) =>
  Renameable (PatBase te f VName)
  where
  rename (PatId v te t pos) =
    PatId <$> applyRename v <*> applyRename te <*> applyRename t <*> pure pos
