module Substitute where

import Data.Bifunctor
import Data.Either
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as S
import Shape
import Syntax

class Substitute a b c where
  substitute :: Map a b -> c -> c

substituteOne :: (Substitute a b c) => (a, b) -> c -> c
substituteOne (a, b) = substitute $ M.singleton a b

instance (Ord c, Substitute a b c) => Substitute a b (Set c) where
  substitute subst = S.map $ substitute subst

instance (Ord c, Substitute a b c) => Substitute a b [c] where
  substitute subst = map $ substitute subst

instance (Substitute a b c) => Substitute a b (Map k c) where
  substitute subst = fmap $ substitute subst

substitute' :: (Ord a, Substitute a b c) => [(a, b)] -> c -> c
substitute' = substitute . M.fromList

instance (Ord v) => Substitute v (Type v) (Type v) where
  substitute subst (TVar v) = fromMaybe (TVar v) $ subst M.!? (unTVar v)
  substitute subst (TArr t s) = TArr (substitute subst t) s
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) =
    Forall pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unTVar pts) subst) t
  substitute subst (DProd pts t) = DProd pts $ substitute subst t
  substitute subst (DSum pts t) = DSum pts $ substitute subst t
  substitute _ t = t

instance (Ord v) => Substitute v (Dim v) (Dim v) where
  substitute subst (DimVar v) = fromMaybe (DimVar v) $ subst M.!? v
  substitute _ (Dim d) = Dim d
  substitute subst (Add ds) = Add $ map (substitute subst) ds

instance (Ord v) => Substitute v (Shape v) (Dim v) where
  substitute subst (DimVar v) =
    case normShape <$> subst M.!? v of
      Just (ShapeDim d) -> substitute subst d
      _ -> DimVar v
  substitute subst (Dim d) = Dim d
  substitute subst (Add ds) = Add $ map (substitute subst) ds

instance (Ord v) => Substitute v (Shape v) (Shape v) where
  substitute subst (ShapeVar v) = fromMaybe (ShapeVar v) $ subst M.!? v
  substitute subst (ShapeDim d) = ShapeDim $ substitute subst d
  substitute subst (Concat shapes) = Concat $ map (substitute subst) shapes

-- instance (Ord v) => Substitute v (Either (Dim v) (Shape v)) (Shape v) where
--  substitute subst (ShapeVar v) = substitute (substToRights subst) (ShapeVar v)
--  substitute subst (ShapeDim d) = ShapeDim $ substitute (substToLefts subst) d
--  substitute subst (Concat shapes) = Concat $ map (substitute subst) shapes
--
-- substToLefts :: (Ord a) => Map a (Either b c) -> Map a b
-- substToLefts =
--  M.fromList . lefts . map (\(a, e) -> bimap (a,) (a,) e) . M.toList
--
-- substToRights :: (Ord a) => Map a (Either b c) -> Map a c
-- substToRights =
--  M.fromList . rights . map (\(a, e) -> bimap (a,) (a,) e) . M.toList
--
-- instance (Ord v) => Substitute v (Either (Dim v) (Shape v)) (Type v) where
--  substitute subst (TArr t s) = TArr t (substitute subst s)
--  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
--  substitute subst (Forall pts t) = Forall pts $ substitute subst t
--  substitute subst (DProd pts t) =
--    DProd pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map fst pts) subst) t
--  substitute subst (DSum pts t) =
--    DSum pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map fst pts) subst) t
--  substitute _ t = t

instance (Ord v) => Substitute v (Shape v) (Type v) where
  substitute subst (TArr t s) = TArr t (substitute subst s)
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) = Forall pts $ substitute subst t
  substitute subst (DProd pts t) =
    DProd pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute subst (DSum pts t) =
    DSum pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute _ t = t
