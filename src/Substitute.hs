{-# LANGUAGE UndecidableInstances #-}

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

instance (Ord v) => Substitute (TVar v) (Type v) (Type v) where
  substitute subst (TVar tvar) = fromMaybe (TVar tvar) $ subst M.!? tvar
  substitute subst (TArr t s) = TArr (substitute subst t) s
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) =
    Forall pts $ substitute (M.filterWithKey (\k _ -> k `notElem` pts) subst) t
  substitute subst (Prod pts t) = Prod pts $ substitute subst t
  substitute subst (Exists pts t) = Exists pts $ substitute subst t
  substitute _ t = t

instance (Ord v) => Substitute v (Dim v) (Dim v) where
  substitute subst (DimVar v) = fromMaybe (DimVar v) $ subst M.!? v
  substitute _ (DimN d) = DimN d
  substitute subst (Add ds) = Add $ map (substitute subst) ds

instance (Ord v) => Substitute v (Shape v) (Dim v) where
  substitute subst (DimVar v) =
    case normShape <$> subst M.!? v of
      Just (ShapeDim d) -> substitute subst d
      _ -> DimVar v
  substitute subst (DimN d) = DimN d
  substitute subst (Add ds) = Add $ map (substitute subst) ds

instance (Ord v) => Substitute v (Dim v) (Shape v) where
  substitute _ (ShapeVar v) = ShapeVar v
  substitute subst (ShapeDim d) = ShapeDim $ substitute subst d
  substitute subst (Concat shapes) = Concat $ map (substitute subst) shapes

instance (Ord v) => Substitute v (Shape v) (Shape v) where
  substitute subst (ShapeVar v) = fromMaybe (ShapeVar v) $ subst M.!? v
  substitute subst (ShapeDim d) = ShapeDim $ substitute subst d
  substitute subst (Concat shapes) = Concat $ map (substitute subst) shapes

instance (Ord v, Substitute v (Dim v) c) => Substitute (IVar v) (Dim v) c where
  substitute subst c = substitute subst' c
    where
      subst' =
        M.mapKeys unpack $
          M.filterWithKey
            ( \k _ -> case k of
                DVar {} -> True
                SVar {} -> False
            )
            subst
      unpack (DVar v) = v
      unpack (SVar v) = v

instance (Ord v, Substitute v (Shape v) c) => Substitute (IVar v) (Shape v) c where
  substitute subst c = substitute subst' c
    where
      subst' =
        M.mapKeys unpack $
          M.filterWithKey
            ( \k _ -> case k of
                DVar {} -> False
                SVar {} -> True
            )
            subst
      unpack (DVar v) = v
      unpack (SVar v) = v

instance (Ord v, Substitute v (Dim z) c, Substitute v (Shape z) c) => Substitute v (Idx z) c where
  substitute subst c =
    substitute substShape $ substitute substDim c
    where
      substDim =
        M.map (fromJust . fromDim) $
          M.filter isDim subst
      substShape =
        M.map (fromJust . fromShape) $
          M.filter isShape subst

instance (Ord v) => Substitute v (Dim v) (Type v) where
  substitute subst (TArr t s) = TArr t (substitute subst s)
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) = Forall pts $ substitute subst t
  substitute subst (Prod pts t) =
    Prod pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute subst (Exists pts t) =
    Exists pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute _ t = t

instance (Ord v) => Substitute v (Shape v) (Type v) where
  substitute subst (TArr t s) = TArr t (substitute subst s)
  substitute subst (ts :-> t) = map (substitute subst) ts :-> substitute subst t
  substitute subst (Forall pts t) = Forall pts $ substitute subst t
  substitute subst (Prod pts t) =
    Prod pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute subst (Exists pts t) =
    Exists pts $ substitute (M.filterWithKey (\k _ -> k `notElem` map unIVar pts) subst) t
  substitute _ t = t
