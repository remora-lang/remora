{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Substitute
  ( Subst (..),
    empty,
    substAtomVar,
    substAtomVars,
    substISpaceVar,
    substTypeExpVar,
    renameVar,
    Substitutable (..),
  )
where

import Data.Bifunctor
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import ISpace
import Syntax

-- | A record of the various kinds of type-level substitutions.
data Subst v = Subst
  { substAtoms :: Map v (AtomType v),
    substShapes :: Map v (Shape v),
    substDims :: Map v (Dim v),
    -- This is probably not what we really want to do (since these are *source*
    -- expressions).  Maybe the right thing is probably to have AST be
    -- parameterized to swap from 'TypeExp' to just 'Type', e.g., after
    -- monomorphization. But, for now this is easier.
    substTypeExps :: Map v (TypeExp v)
  }
  deriving (Show)

instance (Ord v) => Semigroup (Subst v) where
  Subst a1 s1 d1 t1 <> Subst a2 s2 d2 t2 =
    Subst (a1 <> a2) (s1 <> s2) (d1 <> d2) (t1 <> t2)

instance (Ord v) => Monoid (Subst v) where
  mempty = empty

empty :: Subst v
empty = Subst M.empty M.empty M.empty M.empty

substAtomVar :: v -> AtomType v -> Subst v
substAtomVar v t = empty {substAtoms = M.singleton v t}

substAtomVars :: Map v (AtomType v) -> Subst v
substAtomVars m = empty {substAtoms = m}

substISpaceVar :: v -> ISpace v -> Subst v
substISpaceVar v (Dim d) = empty {substDims = M.singleton v d}
substISpaceVar v (Shape s) = empty {substShapes = M.singleton v s}

substTypeExpVar :: v -> TypeExp v -> Subst v
substTypeExpVar v t = empty {substTypeExps = M.singleton v t}

renameVar :: v -> v -> Subst v
renameVar old new =
  Subst
    { substAtoms = M.singleton old $ AtomTypeVar new,
      substShapes = M.singleton old $ ShapeVar new,
      substDims = M.singleton old $ DimVar new,
      substTypeExps = M.empty
    }

without :: (Ord v) => Subst v -> v -> Subst v
without s v =
  Subst
    { substAtoms = M.delete v $ substAtoms s,
      substShapes = M.delete v $ substShapes s,
      substDims = M.delete v $ substDims s,
      substTypeExps = M.delete v $ substTypeExps s
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

instance (Substitutable v a) => Substitutable v (Info a) where
  substitute s (Info a) = Info $ substitute s a

instance (Substitutable v a) => Substitutable v (NE.NonEmpty a) where
  substitute s = fmap (substitute s)

instance (Substitutable v a) => Substitutable v (Maybe a) where
  substitute s = fmap (substitute s)

instance Substitutable v (ISpace v) where
  substitute s (Dim d) = Dim $ substitute s d
  substitute s (Shape sh) = Shape $ substitute s sh

unTypeParamExp :: TypeParamExp v -> v
unTypeParamExp (TEAtomTypeParam v) = v
unTypeParamExp (TEArrayTypeParam v) = v

instance Substitutable v (TypeExp v) where
  substitute s t@(TEAtomVar v _) =
    fromMaybe t $ substTypeExps s M.!? v
  substitute s t@(TEArrayVar v _) =
    fromMaybe t $ substTypeExps s M.!? v
  substitute _ t@TEBool {} = t
  substitute _ t@TEInt {} = t
  substitute _ t@TEFloat {} = t
  substitute s (TEArray t sh pos) =
    TEArray (substitute s t) (substitute s sh) pos
  substitute s (TEArrow a b pos) =
    TEArrow (substitute s a) (substitute s b) pos
  substitute s (TEForall ps t pos) =
    TEForall ps (substitute (foldl without s $ fmap unTypeParamExp ps) t) pos
  substitute s (TEPi ps t pos) =
    TEPi ps (substitute (foldl without s $ fmap unISpaceParam ps) t) pos
  substitute s (TESigma ps t pos) =
    TESigma ps (substitute (foldl without s $ fmap unISpaceParam ps) t) pos

instance Substitutable v (PatBase Info v) where
  substitute s (PatId x te t pos) =
    PatId x (substitute s te) (substitute s t) pos

instance Substitutable v (AtomBase TypeParam Info v) where
  substitute s (Base b t pos) =
    Base b (substitute s t) pos
  substitute s (Lambda pat body t pos) =
    Lambda (substitute s pat) (substitute s body) (substitute s t) pos
  substitute s (TLambda tp body t pos) =
    TLambda
      tp
      (substitute (s `without` unTypeParam tp) body)
      (substitute s t)
      pos
  substitute s (ILambda ip body t pos) =
    ILambda
      ip
      (substitute (s `without` unISpaceParam ip) body)
      (substitute s t)
      pos
  substitute s (Box isp body te t pos) =
    Box (substitute s isp) (substitute s body) (substitute s te) (substitute s t) pos

instance Substitutable v (BindBase TypeParam Info v) where
  substitute s (BindVal x mte e pos) =
    BindVal x (substitute s mte) (substitute s e) pos
  substitute s (BindType tp te t pos) =
    BindType tp (substitute s te) (substitute s t) pos
  substitute s (BindISpace ip isp pos) =
    BindISpace ip (substitute s isp) pos
  substitute s (BindFun x pats mte body t pos) =
    BindFun x (substitute s pats) (substitute s mte) (substitute s body) (substitute s t) pos
  substitute s (BindTFun x tps mte body t pos) =
    let s' = foldl without s $ fmap unTypeParam tps
     in BindTFun x tps (substitute s' mte) (substitute s' body) (substitute s t) pos
  substitute s (BindIFun x ips mte body t pos) =
    let s' = foldl without s $ fmap unISpaceParam ips
     in BindIFun x ips (substitute s' mte) (substitute s' body) (substitute s t) pos

instance Substitutable v (ExpBase TypeParam Info v) where
  substitute s (Var x t pos) =
    Var x (substitute s t) pos
  substitute s (Array dims as t pos) =
    Array dims (substitute s as) (substitute s t) pos
  substitute s (EmptyArray dims te t pos) =
    EmptyArray dims (substitute s te) (substitute s t) pos
  substitute s (Frame dims es t pos) =
    Frame dims (substitute s es) (substitute s t) pos
  substitute s (EmptyFrame dims te t pos) =
    EmptyFrame dims (substitute s te) (substitute s t) pos
  substitute s (App fun arg t pos) =
    App
      (substitute s fun)
      (substitute s arg)
      (bimap (substitute s) (substitute s) <$> t)
      pos
  substitute s (TApp e te t pos) =
    TApp (substitute s e) (substitute s te) (substitute s t) pos
  substitute s (IApp e isp t pos) =
    IApp (substitute s e) (substitute s isp) (substitute s t) pos
  substitute s (Unbox ip x box body t pos) =
    Unbox
      ip
      x
      (substitute s box)
      (substitute (s `without` unISpaceParam ip) body)
      (substitute s t)
      pos
  substitute s (Let bs body t pos) =
    Let (substitute s bs) (substitute s body) (substitute s t) pos
