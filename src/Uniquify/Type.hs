module Uniquify.Type where

import Control.Monad.RWS
import Data.Text (Text)
import Syntax
import Uniquify.Monad
import VName

-- | Uniquify a 'Dim'.
uniquifyDim :: (MonadUniquify m) => Dim Text -> m (Dim VName)
uniquifyDim = uniquifyDim'
  where
    uniquifyDim' (DimVar v) = DimVar <$> fetchDimVar v
    uniquifyDim' (DimN d) = pure $ DimN d
    uniquifyDim' (Add ds) = Add <$> mapM uniquifyDim' ds
    uniquifyDim' (Sub ds) = Sub <$> mapM uniquifyDim' ds
    uniquifyDim' (Mul ds) = Mul <$> mapM uniquifyDim' ds

-- | Uniquify a 'Shape'.
uniquifyShape :: (MonadUniquify m) => Shape Text -> m (Shape VName)
uniquifyShape = uniquifyShape'
  where
    uniquifyShape' (ShapeVar v) = ShapeVar <$> fetchShapeVar v
    uniquifyShape' (ShapeDim d) = ShapeDim <$> uniquifyDim d
    uniquifyShape' (Concat ss) = Concat <$> mapM uniquifyShape' ss

-- | Uniquify an `Extent`.
uniquifyExtent :: (MonadUniquify m) => Extent Text -> m (Extent VName)
uniquifyExtent = mapExtent (fmap Dim . uniquifyDim) (fmap Shape . uniquifyShape)

-- | Uniquify a 'Type'.
uniquifyType :: (MonadUniquify m) => Type Text -> m (Type VName)
uniquifyType = uniquifyType'
  where
    uniquifyType' (ArrayType t) = ArrayType <$> uniquifyArrayType t
    uniquifyType' (AtomType t) = AtomType <$> uniquifyAtomType t

uniquifyArrayType :: (MonadUniquify m) => ArrayType Text -> m (ArrayType VName)
uniquifyArrayType (A t shape) =
  A <$> uniquifyAtomType t <*> uniquifyShape shape

uniquifyAtomType :: (MonadUniquify m) => AtomType Text -> m (AtomType VName)
uniquifyAtomType (AtomTypeVar v) =
  AtomTypeVar <$> fetchAtomTypeVar v
uniquifyAtomType Bool = pure Bool
uniquifyAtomType Int = pure Int
uniquifyAtomType Float = pure Float
uniquifyAtomType (as :-> b) =
  (:->) <$> mapM uniquifyArrayType as <*> uniquifyArrayType b
uniquifyAtomType (Forall pts t) =
  binds withTypeParam pts $ \pts' ->
    Forall pts' <$> uniquifyArrayType t
uniquifyAtomType (Pi pts t) =
  binds withExtentParam pts $ \pts' ->
    Pi pts' <$> uniquifyArrayType t
uniquifyAtomType (Sigma pts t) = do
  binds withExtentParam pts $ \pts' -> do
    Sigma pts' <$> uniquifyArrayType t
