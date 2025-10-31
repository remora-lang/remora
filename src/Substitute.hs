{-# LANGUAGE UndecidableInstances #-}

module Substitute where

import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Shape
import Syntax

-- | Applies a substitution from @a@ to @b@ on a @c@.
class Substitute a b c where
  substitute :: Map a b -> c -> c

-- | Apply a single substitution.
substituteOne :: (Substitute a b c) => (a, b) -> c -> c
substituteOne (a, b) = substitute $ M.singleton a b

substitute' :: (Ord a, Substitute a b c, Foldable t) => t (a, b) -> c -> c
substitute' = substitute . M.fromList . toList

instance (Ord c, Substitute a b c) => Substitute a b (Set c) where
  substitute subst = S.map $ substitute subst

instance (Ord c, Substitute a b c) => Substitute a b [c] where
  substitute subst = map $ substitute subst

instance (Substitute a b c) => Substitute a b (Map k c) where
  substitute subst = fmap $ substitute subst

instance (Ord v) => Substitute v v v where
  substitute subst v = fromMaybe v $ subst M.!? v

-- == 'Type' substitutions
--------------------------------------------------------------------------------

instance (Ord v) => Substitute v v (Info v) where
  substitute subst (Info v) =
    maybe (Info v) Info $ subst M.!? v

instance (Ord v) => Substitute v (ScalarType f v) (ScalarType f v) where
  substitute subst t@(ScalarTVar v) =
    fromMaybe t $ subst M.!? v
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) =
    Forall pts $
      substitute (M.filterWithKey (\k _ -> k `notElem` mapMaybe fromAtomTVar pts) subst) t
  substitute subst (Pi pts t) = Pi pts $ substitute subst t
  substitute subst (Sigma pts t) = Sigma pts $ substitute subst t
  substitute _ t = t

instance (Ord v) => Substitute v (ScalarType f v) (ArrayType f v) where
  substitute subst (A t s) = A (substitute subst t) s
  substitute _ t = t

instance (Eq v, Ord v, Substitute v c c) => Substitute (TVar v) c c where
  substitute subst = substitute $ M.mapKeys unTVar subst

instance (Eq v, Ord v, Substitute v v (f v)) => Substitute v v (ArrayType f v) where
  substitute subst (A t s) = A (substitute subst t) (substitute subst s)
  substitute subst (ArrayTypeVar t et s) =
    ArrayTypeVar
      (substitute subst t)
      (substitute subst et)
      (substitute subst s)

instance (Eq v, Ord v, Substitute v v (f v)) => Substitute v v (ScalarType f v) where
  substitute subst t@(ScalarTVar v) =
    maybe t ScalarTVar $ subst M.!? v
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) =
    Forall pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unTVar pts) subst) t
  substitute subst (Pi pts t) =
    Pi pts $ substitute subst t
  substitute subst (Sigma pts t) =
    Sigma pts $ substitute subst t
  substitute _ t = t

--
-- This is risky; probably should define the proper instance.
instance (Eq a, Ord a, Substitute a b c) => Substitute (TVar a) (TVar b) c where
  substitute subst = substitute' [(unTVar k, unTVar v) | (k, v) <- M.toList subst]

-- == 'Dim' and 'Shape' substitutions
--------------------------------------------------------------------------------

instance (Ord v) => Substitute v (Dim v) (Dim v) where
  substitute subst (DimVar v) = fromMaybe (DimVar v) $ subst M.!? v
  substitute _ (DimN d) = DimN d
  substitute subst (Add ds) = Add $ map (substitute subst) ds

instance (Ord v) => Substitute v (Shape v) (Shape v) where
  substitute subst (ShapeVar v) = fromMaybe (ShapeVar v) $ subst M.!? v
  substitute subst (ShapeDim (DimVar v)) =
    case normShape <$> (subst M.!? v) of
      Just (ShapeDim d) -> ShapeDim d
      Just _ ->
        error $
          "substitute: invalid substition maps dimension variable to a shape"
      Nothing -> ShapeDim (DimVar v)
  substitute _ (ShapeDim d) = ShapeDim d
  substitute subst (Concat shapes) = Concat $ map (substitute subst) shapes

instance (Eq v, Ord v) => Substitute v v (Dim v) where
  substitute subst (DimVar v) = DimVar $ substitute subst v
  substitute _ (DimN d) = DimN d
  substitute subst (Add ds) = Add $ map (substitute subst) ds

instance (Eq v, Ord v) => Substitute v v (Shape v) where
  substitute subst (ShapeVar v) = ShapeVar $ substitute subst v
  substitute subst (ShapeDim d) = ShapeDim $ substitute subst d
  substitute subst (Concat shapes) = Concat $ map (substitute subst) shapes

instance (Ord v) => Substitute v (Dim v) (Shape v) where
  substitute _ (ShapeVar v) = ShapeVar v
  substitute subst (ShapeDim d) = ShapeDim $ substitute subst d
  substitute subst (Concat shapes) = Concat $ map (substitute subst) shapes

instance (Ord v) => Substitute v (Shape v) (ArrayType f v) where
  substitute subst (A t s) = A (substitute subst t) (substitute subst s)

instance (Ord v) => Substitute v (Shape v) (ScalarType f v) where
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) = Forall pts $ substitute subst t
  substitute subst (Pi pts t) =
    Pi pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute subst (Sigma pts t) =
    Sigma pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute _ t = t

instance (Ord v) => Substitute v (Dim v) (ArrayType f v) where
  substitute subst (A t s) = A (substitute subst t) (substitute subst s)

instance (Ord v) => Substitute v (Dim v) (ScalarType f v) where
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) = Forall pts $ substitute subst t
  substitute subst (Pi pts t) =
    Pi pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute subst (Sigma pts t) =
    Sigma pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute _ t = t

instance (Ord v) => Substitute (IVar v) (IVar v) (Shape v) where
  substitute subst = substitute' [(unIVar k, unIVar v) | (k, v) <- M.toList subst]

instance (Ord v) => Substitute (IVar v) (IVar v) (ArrayType f v) where
  substitute subst (A t s) = A (substitute subst t) (substitute subst s)

instance (Ord v) => Substitute (IVar v) (IVar v) (ScalarType f v) where
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) = Forall pts $ substitute subst t
  substitute subst (Pi pts t) =
    Pi pts $ substitute (M.filterWithKey (\k _ -> k `notElem` pts) subst) t
  substitute subst (Sigma pts t) =
    Sigma pts $ substitute (M.filterWithKey (\k _ -> k `notElem` pts) subst) t
  substitute _ t = t

instance (Ord v) => Substitute (IVar v) (Idx v) (ArrayType f v) where
  substitute subst (A t s) = A (substitute subst t) (substitute subst s)

instance (Ord v) => Substitute (IVar v) (Idx v) (ScalarType f v) where
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) = Forall pts $ substitute subst t
  substitute subst (Pi pts t) =
    Pi pts $ substitute (M.filterWithKey (\k _ -> k `notElem` pts) subst) t
  substitute subst (Sigma pts t) =
    Sigma pts $ substitute (M.filterWithKey (\k _ -> k `notElem` pts) subst) t
  substitute _ t = t

instance (Ord v, Substitute v (Dim v) c, Substitute v (Shape v) c) => Substitute v (Idx v) c where
  substitute subst c =
    substitute substShape $ substitute substDim c
    where
      substDim =
        M.map (fromJust . fromDim) $
          M.filter isDim subst
      substShape =
        M.map (fromJust . fromShape) $
          M.filter isShape subst

instance (Ord v) => Substitute (IVar v) (Idx v) (Shape v) where
  substitute subst = substitute $ M.mapKeys unIVar subst
