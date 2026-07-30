{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Computes free term, type, and index variables.
module Free
  ( FreeVars (..),
    Free (..),
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Syntax

data FreeVars v = FreeVars
  { freeTermVars :: Set v,
    freeTypeVars :: Set v,
    freeISpaceVars :: Set v
  }
  deriving (Show)

instance (Ord v) => Semigroup (FreeVars v) where
  FreeVars ts1 tys1 is1 <> FreeVars ts2 tys2 is2 =
    FreeVars (ts1 <> ts2) (tys1 <> tys2) (is1 <> is2)

instance (Ord v) => Monoid (FreeVars v) where
  mempty = noFreeVars

noFreeVars :: FreeVars v
noFreeVars = FreeVars S.empty S.empty S.empty

termVar :: v -> FreeVars v
termVar v = noFreeVars {freeTermVars = S.singleton v}

typeVar :: v -> FreeVars v
typeVar v = noFreeVars {freeTypeVars = S.singleton v}

ispaceVar :: v -> FreeVars v
ispaceVar v = noFreeVars {freeISpaceVars = S.singleton v}

boundTerm :: (Ord v) => v -> FreeVars v -> FreeVars v
boundTerm v free = free {freeTermVars = S.delete v $ freeTermVars free}

boundTerms :: (Ord v, Foldable t) => t v -> FreeVars v -> FreeVars v
boundTerms = flip $ foldr boundTerm

boundType :: (Ord v) => v -> FreeVars v -> FreeVars v
boundType v free = free {freeTypeVars = S.delete v $ freeTypeVars free}

boundTypes :: (Ord v, Foldable t) => t v -> FreeVars v -> FreeVars v
boundTypes = flip $ foldr boundType

boundTypeParam :: (Ord v) => TypeParam v -> FreeVars v -> FreeVars v
boundTypeParam = boundType . unTypeParam

boundISpaceParam :: (Ord v) => ISpaceParam v -> FreeVars v -> FreeVars v
boundISpaceParam p free =
  free {freeISpaceVars = S.delete (unISpaceParam p) $ freeISpaceVars free}

boundISpaceParams ::
  (Ord v, Foldable t) => t (ISpaceParam v) -> FreeVars v -> FreeVars v
boundISpaceParams = flip $ foldr boundISpaceParam

boundByBind ::
  (Ord v, Foldable tp) =>
  BindBase te tp f v ->
  FreeVars v ->
  FreeVars v
boundByBind (BindVal v _ _ _) = boundTerm v
boundByBind (BindFun v _ _ _ _ _) = boundTerm v
boundByBind (BindTFun v _ _ _ _ _) = boundTerm v
boundByBind (BindIFun v _ _ _ _ _) = boundTerm v
boundByBind (BindType tp _ _ _) = boundTypes $ toList tp
boundByBind (BindISpace ip _ _) = boundISpaceParam ip

class Free v a | a -> v where
  freeVars :: (Ord v) => a -> FreeVars v

instance (Free v a) => Free v [a] where
  freeVars = foldMap freeVars

instance (Free v a) => Free v (NE.NonEmpty a) where
  freeVars = foldMap freeVars

instance (Free v a) => Free v (Maybe a) where
  freeVars = foldMap freeVars

instance (Free v a) => Free v (Info a) where
  freeVars (Info a) = freeVars a

instance (Free v a) => Free v (NoInfo a) where
  freeVars NoInfo = mempty

instance (Free v a, Free v b) => Free v (a, b) where
  freeVars (a, b) = freeVars a <> freeVars b

instance Free v (Dim v) where
  freeVars (DimVar v) = ispaceVar v
  freeVars (DimN _) = mempty
  freeVars (Add ds) = freeVars ds
  freeVars (Mul ds) = freeVars ds
  freeVars (Sub ds) = freeVars ds

instance Free v (Shape v) where
  freeVars (ShapeVar v) = ispaceVar v
  freeVars (ShapeDim d) = freeVars d
  freeVars (Concat ss) = freeVars ss

instance Free v (ISpace v) where
  freeVars (Dim d) = freeVars d
  freeVars (Shape s) = freeVars s

instance Free v (AtomType v) where
  freeVars (AtomTypeVar v) = typeVar v
  freeVars Bool = mempty
  freeVars Int = mempty
  freeVars Float = mempty
  freeVars (a :-> b) = freeVars a <> freeVars b
  freeVars (Forall p t) = boundTypeParam p $ freeVars t
  freeVars (Pi p t) = boundISpaceParam p $ freeVars t
  freeVars (Sigma p t) = boundISpaceParam p $ freeVars t
  freeVars (Record fs) = foldMap (freeVars . snd) fs

instance Free v (ArrayType v) where
  freeVars (t :@ s) = freeVars t <> freeVars s

instance Free v (Type v) where
  freeVars (AtomType t) = freeVars t
  freeVars (ArrayType t) = freeVars t

instance Free v (TypeExp v) where
  freeVars (TEAtomVar v _) = typeVar v
  freeVars (TEArrayVar v _) = typeVar v
  freeVars TEBool {} = mempty
  freeVars TEInt {} = mempty
  freeVars TEFloat {} = mempty
  freeVars (TEArray t s _) = freeVars t <> freeVars s
  freeVars (TEArrow a b _) = freeVars a <> freeVars b
  freeVars (TEForall ps t _) = boundTypes (foldMap toList ps) $ freeVars t
  freeVars (TEPi ps t _) = boundISpaceParams ps $ freeVars t
  freeVars (TESigma ps t _) = boundISpaceParams ps $ freeVars t
  freeVars (TERecord fs _) = foldMap (freeVars . snd) fs

instance
  ( Free v (te v),
    forall a. (Free v a) => Free v (f a)
  ) =>
  Free v (PatBase te f v)
  where
  freeVars (PatId _ te t _) = freeVars te <> freeVars t

instance
  ( Foldable tp,
    Free v (te v),
    forall a. (Free v a) => Free v (f a)
  ) =>
  Free v (AtomBase te tp f v)
  where
  freeVars (Base _ t _) = freeVars t
  freeVars (Lambda pat body t _) =
    freeVars pat <> boundTerm (patVar pat) (freeVars body) <> freeVars t
  freeVars (TLambda tp body t _) =
    boundTypes (toList tp) (freeVars body) <> freeVars t
  freeVars (ILambda ip body t _) =
    boundISpaceParam ip (freeVars body) <> freeVars t
  freeVars (Box isp body te t _) =
    freeVars isp <> freeVars body <> freeVars te <> freeVars t

instance
  ( Foldable tp,
    Free v (te v),
    forall a. (Free v a) => Free v (f a)
  ) =>
  Free v (BindBase te tp f v)
  where
  freeVars (BindVal _ mte e _) = freeVars mte <> freeVars e
  freeVars (BindType _ te t _) = freeVars te <> freeVars t
  freeVars (BindISpace _ isp _) = freeVars isp
  freeVars (BindFun _ pats mte body t _) =
    freeVars pats
      <> boundTerms (patVar <$> pats) (freeVars body)
      <> freeVars mte
      <> freeVars t
  freeVars (BindTFun _ tps mte body t _) =
    boundTypes (foldMap toList tps) (freeVars mte <> freeVars body)
      <> freeVars t
  freeVars (BindIFun _ ips mte body t _) =
    boundISpaceParams ips (freeVars mte <> freeVars body)
      <> freeVars t

instance
  ( Foldable tp,
    Free v (te v),
    forall a. (Free v a) => Free v (f a)
  ) =>
  Free v (ExpBase te tp f v)
  where
  freeVars (Var x t _) = termVar x <> freeVars t
  freeVars (Array _ as t _) = freeVars as <> freeVars t
  freeVars (EmptyArray _ te t _) = freeVars te <> freeVars t
  freeVars (Frame _ es t _) = freeVars es <> freeVars t
  freeVars (EmptyFrame _ te t _) = freeVars te <> freeVars t
  freeVars (App f arg t _) = freeVars f <> freeVars arg <> freeVars t
  freeVars (TApp e te t _) = freeVars e <> freeVars te <> freeVars t
  freeVars (IApp e isp t _) = freeVars e <> freeVars isp <> freeVars t
  freeVars (Unbox ip x box body t _) =
    freeVars box
      <> boundISpaceParam ip (boundTerm x $ freeVars body)
      <> freeVars t
  freeVars (Let bs body t _) =
    foldr (\b acc -> freeVars b <> boundByBind b acc) (freeVars body) bs
      <> freeVars t
  freeVars (Struct fs t _) =
    foldMap (\(_, shape, e) -> freeVars shape <> freeVars e) fs <> freeVars t
  freeVars (FieldProj e _ t _) = freeVars e <> freeVars t

instance
  ( Foldable tp,
    Free v (te v),
    forall a. (Free v a) => Free v (f a)
  ) =>
  Free v (DeclBase te tp f v)
  where
  freeVars (Def b) = freeVars b
  freeVars (Entry _ params mte body t _) =
    freeVars params
      <> boundTerms (map patVar params) (freeVars body)
      <> freeVars mte
      <> freeVars t

instance
  ( Foldable tp,
    Free v (te v),
    forall a. (Free v a) => Free v (f a)
  ) =>
  Free v (ProgBase te tp f v)
  where
  freeVars (Prog decs) =
    boundTerms (mapMaybe declName decs) $ freeVars decs
