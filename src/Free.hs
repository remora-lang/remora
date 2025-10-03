module Free where

import Data.Set (Set)
import Data.Set qualified as S
import Shape

class Free v x where
  free :: x -> Set v

instance (Ord v) => Free v (Dim v) where
  free (DimVar v) = S.singleton v
  free Dim {} = S.empty
  free (Add ds) = foldMap free ds

instance (Ord v) => Free v (Shape v) where
  free (ShapeVar v) = S.singleton v
  free (ShapeDim d) = free d
  free (Concat ss) = foldMap free ss
