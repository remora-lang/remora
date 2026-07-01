{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Substitute
  ( Subst (..),
    empty,
    substAtomVar,
    substAtomVars,
    substISpaceVar,
    renameVar,
    Substitutable (..),
  )
where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import ISpace
import Syntax

-- | A record of the various kinds of type-level substitutions.
data Subst v = Subst
  { substAtoms :: Map v (AtomType v),
    substShapes :: Map v (Shape v),
    substDims :: Map v (Dim v)
  }
  deriving (Show)

instance (Ord v) => Semigroup (Subst v) where
  Subst a1 s1 d1 <> Subst a2 s2 d2 =
    Subst (a1 <> a2) (s1 <> s2) (d1 <> d2)

instance (Ord v) => Monoid (Subst v) where
  mempty = empty

empty :: Subst v
empty = Subst M.empty M.empty M.empty

substAtomVar :: v -> AtomType v -> Subst v
substAtomVar v t = empty {substAtoms = M.singleton v t}

substAtomVars :: Map v (AtomType v) -> Subst v
substAtomVars m = empty {substAtoms = m}

substISpaceVar :: v -> ISpace v -> Subst v
substISpaceVar v (Dim d) = empty {substDims = M.singleton v d}
substISpaceVar v (Shape s) = empty {substShapes = M.singleton v s}

renameVar :: v -> v -> Subst v
renameVar old new =
  Subst
    { substAtoms = M.singleton old $ AtomTypeVar new,
      substShapes = M.singleton old $ ShapeVar new,
      substDims = M.singleton old $ DimVar new
    }

without :: (Ord v) => Subst v -> v -> Subst v
without s v =
  Subst
    { substAtoms = M.delete v $ substAtoms s,
      substShapes = M.delete v $ substShapes s,
      substDims = M.delete v $ substDims s
    }

class Substitutable v a | a -> v where
  substitute :: (Ord v) => Subst v -> a -> a

instance (Substitutable v a) => Substitutable v [a] where
  substitute = map . substitute

instance Substitutable v (Dim v) where
  substitute s (DimVar v) =
    fromMaybe (DimVar v) $ substDims s M.!? v
  substitute _ (DimN n) = DimN n
  substitute s (Add ds) = Add $ substitute s ds
  substitute s (Sub ds) = Sub $ substitute s ds
  substitute s (Mul ds) = Mul $ substitute s ds

instance Substitutable v (Shape v) where
  substitute s (ShapeVar v) =
    fromMaybe (ShapeVar v) $ substShapes s M.!? v
  substitute s (ShapeDim d) = ShapeDim $ substitute s d
  substitute s (Concat ss) = Concat $ substitute s ss

instance Substitutable v (AtomType v) where
  substitute s t@(AtomTypeVar v) =
    fromMaybe t $ substAtoms s M.!? v
  substitute _ Bool = Bool
  substitute _ Int = Int
  substitute _ Float = Float
  substitute s (a :-> b) =
    substitute s a :-> substitute s b
  substitute s (Forall p t) =
    Forall p $
      substitute (s `without` unTypeParam p) t
  substitute s (Pi p t) =
    Pi p $
      substitute (s `without` unISpaceParam p) t
  substitute s (Sigma p t) =
    Sigma p $
      substitute (s `without` unISpaceParam p) t

instance Substitutable v (ArrayType v) where
  substitute s (A t shape) = A (substitute s t) (substitute s shape)

instance Substitutable v (Type v) where
  substitute s (AtomType t) = AtomType $ substitute s t
  substitute s (ArrayType t) = ArrayType $ substitute s t
