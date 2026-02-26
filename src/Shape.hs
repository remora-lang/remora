module Shape
  ( Dim (..),
    Shape (..),
    Extent (..),
    extentToShape,
    mapExtent,
    fromDim,
    isDim,
    fromShape,
    isShape,
    ExtentParam (..),
    unExtentParam,
    coerceToDim,
    normDim,
    normShape,
    intsToShape,
  )
where

import Data.Bifunctor
import Data.List (sort)
import Data.Maybe
import Prettyprinter
import Util (prettyString)

-- | Dimensions.
data Dim v
  = -- | A dimension variable.
    DimVar v
  | -- | A numeric dimension.
    DimN Int
  | -- | Addition of dimensions.
    Add [Dim v]
  | -- | Multiplication of dimensions.
    Mul [Dim v]
  | -- | Subtraction of dimensions.
    Sub [Dim v]
  deriving (Show, Eq, Ord)

instance (Show v, Pretty v) => Pretty (Dim v) where
  pretty (DimVar v) = "$" <> pretty v
  pretty (DimN d) = pretty d
  pretty (Add ds) = parens $ hsep ("+" : map pretty ds)
  pretty (Mul ds) = parens $ hsep ("*" : map pretty ds)
  pretty (Sub ds) = parens $ hsep ("-" : map pretty ds)

instance Semigroup (Dim v) where
  d <> e = Add [d, e]

instance Monoid (Dim v) where
  mempty = DimN 0

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
  mempty = Concat mempty

instance (Show v, Pretty v) => Pretty (Shape v) where
  pretty (ShapeVar v) = "@" <> pretty v
  pretty (ShapeDim d) = pretty d
  pretty (Concat []) = "•"
  pretty (Concat is) = parens $ hsep ("++" : map pretty is)

-- | An 'Extent', which is either a 'Dim' or a 'Shape'.
data Extent v
  = Dim (Dim v)
  | Shape (Shape v)
  deriving (Eq, Show, Ord)

instance (Show v, Pretty v) => Pretty (Extent v) where
  pretty (Shape s) = pretty s
  pretty (Dim d) = pretty d

extentToShape :: Extent v -> Shape v
extentToShape (Dim d) = ShapeDim d
extentToShape (Shape s) = s

mapExtent :: (Dim v -> a) -> (Shape v -> a) -> Extent v -> a
mapExtent f_dim f_shape extent =
  case extent of
    Dim d -> f_dim d
    Shape s -> f_shape s

fromDim :: Extent v -> Maybe (Dim v)
fromDim (Dim d) = pure d
fromDim _ = Nothing

isDim :: Extent v -> Bool
isDim = isJust . fromDim

fromShape :: Extent v -> Maybe (Shape v)
fromShape (Shape s) = pure s
fromShape _ = Nothing

isShape :: Extent v -> Bool
isShape = isJust . fromShape

-- | An extent parameter, which is either a shape parameter or a dimension
-- parameter.
data ExtentParam v
  = -- | A shape variable.
    ShapeParam v
  | -- | A dimension variable.
    DimParam v
  deriving (Show, Eq, Ord, Functor)

instance (Show v, Pretty v) => Pretty (ExtentParam v) where
  pretty (ShapeParam v) = "@" <> pretty v
  pretty (DimParam v) = "$" <> pretty v

-- | Extract the variable out of an 'ExtentParam'.
unExtentParam :: ExtentParam v -> v
unExtentParam (ShapeParam v) = v
unExtentParam (DimParam v) = v

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
        Sub ds -> second (Sub ds :)
        Mul ds -> second (Mul ds :)
        _ -> error ""
normDim (Mul ds) =
  case (d, vars) of
    (_, []) -> DimN d
    _ -> Mul (DimN d : sort vars)
  where
    (d, vars) =
      foldr discriminate (1, []) $
        flip concatMap ds $ \d' ->
          case normDim d' of
            Mul ds' -> ds'
            d'' -> pure d''
    discriminate d' =
      case d' of
        DimN n -> first (* n)
        DimVar v -> second (DimVar v :)
        Add ds -> second (Add ds :)
        Sub ds -> second (Sub ds :)
        _dErr -> error "error: "
normDim (Sub ds) = Sub $ map normDim ds

-- | Basic shape normalization; normalizes dimensions and flattens
-- concatenations and returns the result in sorted order.
normShape :: (Ord v, Eq v) => Shape v -> Shape v
normShape (ShapeVar v) = ShapeVar v
normShape (ShapeDim d) = ShapeDim $ normDim d
normShape (Concat ss) =
  case merged of
    [s] -> s
    _ -> Concat merged
  where
    merged =
      flip concatMap ss $ \s ->
        case normShape s of
          Concat ss' -> ss'
          s' -> pure s'

-- | Turns an explicit list of dimensions into a 'Shape'.
intsToShape :: [Int] -> Shape v
intsToShape ds = Concat $ map (ShapeDim . DimN) ds
