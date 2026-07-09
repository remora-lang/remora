module Uniquify.Type where

import Data.Text (Text)
import Syntax
import Uniquify.Monad
import VName

uniquifyDim :: (MonadUniquify m) => Dim Text -> m (Dim VName)
uniquifyDim = uniquifyDim'
  where
    uniquifyDim' (DimVar v) = DimVar <$> fetchDimVar v
    uniquifyDim' (DimN d) = pure $ DimN d
    uniquifyDim' (Add ds) = Add <$> mapM uniquifyDim' ds
    uniquifyDim' (Sub ds) = Sub <$> mapM uniquifyDim' ds
    uniquifyDim' (Mul ds) = Mul <$> mapM uniquifyDim' ds

uniquifyShape :: (MonadUniquify m) => Shape Text -> m (Shape VName)
uniquifyShape = uniquifyShape'
  where
    uniquifyShape' (ShapeVar v) = ShapeVar <$> fetchShapeVar v
    uniquifyShape' (ShapeDim d) = ShapeDim <$> uniquifyDim d
    uniquifyShape' (Concat ss) = Concat <$> mapM uniquifyShape' ss

uniquifyISpace :: (MonadUniquify m) => ISpace Text -> m (ISpace VName)
uniquifyISpace = mapISpace (fmap Dim . uniquifyDim) (fmap Shape . uniquifyShape)

uniquifyType :: (MonadUniquify m) => Type Text -> m (Type VName)
uniquifyType = uniquifyType'
  where
    uniquifyType' (ArrayType t) = ArrayType <$> uniquifyArrayType t
    uniquifyType' (AtomType t) = AtomType <$> uniquifyAtomType t

uniquifyArrayType :: (MonadUniquify m) => ArrayType Text -> m (ArrayType VName)
uniquifyArrayType (t :@ shape) =
  (:@) <$> uniquifyAtomType t <*> uniquifyShape shape

uniquifyAtomType :: (MonadUniquify m) => AtomType Text -> m (AtomType VName)
uniquifyAtomType (AtomTypeVar v) =
  AtomTypeVar <$> fetchAtomTypeVar v
uniquifyAtomType Bool = pure Bool
uniquifyAtomType Int = pure Int
uniquifyAtomType Float = pure Float
uniquifyAtomType (a :-> b) =
  (:->) <$> uniquifyArrayType a <*> uniquifyArrayType b
uniquifyAtomType (Forall pt t) =
  withTypeParam pt $ \pt' ->
    Forall pt' <$> uniquifyArrayType t
uniquifyAtomType (Pi pt t) =
  withISpaceParam pt $ \pt' ->
    Pi pt' <$> uniquifyArrayType t
uniquifyAtomType (Sigma pt t) =
  withISpaceParam pt $ \pt' ->
    Sigma pt' <$> uniquifyArrayType t
