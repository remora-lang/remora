module Shape
  ( Dim (..),
    Shape (..),
    Idx (..),
    mapIdx,
    fromDim,
    isDim,
    fromShape,
    isShape,
    IVar (..),
    unIVar,
    coerceToDim,
    normDim,
    normShape,
    (\\),
    intsToShape,
    (.<=.),
    maximumShape,
  )
where

import Data.Bifunctor
import Data.List (sort)
import Data.Maybe
import Prettyprinter

-- | Dimensions.
data Dim v
  = -- | A dimension variable.
    DimVar v
  | -- | A numeric dimension.
    DimN Int
  | -- | Addition of dimensions.
    Add [Dim v]
  deriving (Show, Eq, Ord)

instance (Show v, Pretty v) => Pretty (Dim v) where
  pretty (DimVar v) = pretty v
  pretty (DimN d) = pretty d
  pretty (Add ds) = parens $ hsep ("+" : map pretty ds)

-- | Shapes.
data Shape v
  = -- | Shape variables.
    ShapeVar v
  | -- | A shape consisting of a single dimension.
    ShapeDim (Dim v)
  | -- | Concatenation of shapes.
    Concat [Shape v]
  deriving (Show, Eq, Ord)

instance Semigroup (Shape v) where
  s <> t = Concat [s, t]

instance Monoid (Shape v) where
  mempty = Concat []

instance (Show v, Pretty v) => Pretty (Shape v) where
  pretty (ShapeVar v) = pretty v
  pretty (ShapeDim d) = pretty d
  pretty (Concat []) = "•"
  pretty (Concat is) = parens $ hsep ("++" : map pretty is)

-- TODO: Remove
instance (Show v, Pretty v) => Pretty (Either (Dim v) (Shape v)) where
  pretty = either pretty pretty

-- | An type index. Since shape variables and dimension variables can be
-- statically distinguished, we don't need a constructor for type index
-- variables here.
data Idx v
  = Dim (Dim v)
  | Shape (Shape v)
  deriving (Eq, Show, Ord)

instance (Show v, Pretty v) => Pretty (Idx v) where
  pretty (Shape s) = pretty s
  pretty (Dim d) = pretty d

mapIdx :: (Dim v -> a) -> (Shape v -> a) -> Idx v -> a
mapIdx f_dim f_shape idx =
  case idx of
    Dim d -> f_dim d
    Shape s -> f_shape s

fromDim :: Idx v -> Maybe (Dim v)
fromDim (Dim d) = pure d
fromDim _ = Nothing

isDim :: Idx v -> Bool
isDim = isJust . fromDim

fromShape :: Idx v -> Maybe (Shape v)
fromShape (Shape s) = pure s
fromShape _ = Nothing

isShape :: Idx v -> Bool
isShape = isJust . fromShape

-- | Type index variables. These are needed for type index parameters and
-- patterns.
data IVar v
  = -- | A shape variable.
    SVar v
  | -- | A dimension variable.
    DVar v
  deriving (Show, Eq, Ord, Functor)

instance (Show v, Pretty v) => Pretty (IVar v) where
  pretty (SVar v) = "@" <> pretty v
  pretty (DVar v) = "$" <> pretty v

-- | Extract the variable out of an 'IVar'.
unIVar :: IVar v -> v
unIVar (SVar v) = v
unIVar (DVar v) = v

-- | Coerce a shape into a dimension; fails for shapes that don't have rank 1;
-- optimistically will coerce shape variables. Assumes a normalized shape.
coerceToDim :: Shape v -> Maybe (Dim v)
coerceToDim (ShapeVar v) = pure $ DimVar v
coerceToDim (ShapeDim d) = pure d
coerceToDim _ = Nothing

-- | Basic dimension normalization; collects all numeric dimensions to the front
-- of an 'Add' and plops any variables after in sorted order.
normDim :: (Ord v, Eq v) => Dim v -> Dim v
normDim (DimN n) = DimN n
normDim (DimVar v) = DimVar v
normDim (Add ds) =
  case (d, vars) of
    (_, []) -> DimN d
    _ -> Add (DimN d : sort vars)
  where
    (d, vars) =
      foldr discriminate (0, []) $
        flip concatMap ds $ \d' ->
          case normDim d' of
            Add ds' -> ds'
            d'' -> pure d''
    discriminate d' =
      case d' of
        DimN n -> first (+ n)
        DimVar v -> second (DimVar v :)
        _ -> error ""

-- | Basic shape normalization; normalizes dimensions and flattens
-- concatenations and returns the result in sorted order.
normShape :: (Ord v, Eq v) => Shape v -> Shape v
normShape (ShapeVar v) = ShapeVar v
normShape (ShapeDim d) = ShapeDim $ normDim d
normShape (Concat ss) =
  case merged of
    [s] -> s
    _ -> Concat $ sort merged
  where
    merged =
      flip concatMap ss $ \s ->
        case normShape s of
          Concat ss' -> ss'
          s' -> pure s'

-- | Shape suffix subtraction; given shapes @(++ s1 s2)@ and @t@ if @t == s2@ then
-- returns @Just s1@. Otherwise fails with @Nothing@.
(\\) :: (Eq v, Show v) => Shape v -> Shape v -> Maybe (Shape v)
s \\ t
  | s == t = Just mempty
Concat [] \\ _ = Nothing
s \\ Concat [] = pure s
(Concat ss) \\ (Concat ts)
  | last ss == last ts = Concat (init ss) \\ Concat (init ts)
(Concat ss) \\ t
  | last ss == t = pure $ Concat $ init ss
s \\ t = error $ show (s, t)

-- | Turns an explicit list of dimensions into a 'Shape'.
intsToShape :: [Int] -> Shape v
intsToShape ds = Concat $ map (ShapeDim . DimN) ds

-- | @s .<= t@ is true if @s@ is a suffix of @t@.
(.<=.) :: (Eq v, Show v) => Shape v -> Shape v -> Bool
s .<=. t = isJust $ t \\ s

-- | Returns the largest shape from a collection of shapes with a common prefix.
-- Unsafe to use if the shapes do not have a common prefix.
maximumShape :: (Ord v, Show v, Pretty v, Foldable t) => t (Shape v) -> Shape v
maximumShape =
  foldr
    ( \next shape ->
        if shape .<=. next
          then next
          else shape
    )
    mempty
    . foldMap ((\x -> [x]) . normShape)
