module ISpace
  ( Dim (..),
    Shape (..),
    ISpace (..),
    ispaceToShape,
    mapISpace,
    fromDim,
    isDim,
    fromShape,
    isShape,
    ISpaceParam (..),
    unISpaceParam,
    coerceToDim,
    normDim,
    normShape,
    peelShape,
    intsToShape,
    dimToInt,
    shapeToInts,
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
  pretty s@Concat {} = brackets $ hsep $ map pretty $ flattenShape s
    where
      flattenShape :: Shape v -> [Shape v]
      flattenShape (Concat ss) = concatMap flattenShape ss
      flattenShape s = pure s

-- | An 'ISpace', which is either a 'Dim' or a 'Shape'.
data ISpace v
  = Dim (Dim v)
  | Shape (Shape v)
  deriving (Eq, Show, Ord)

instance (Show v, Pretty v) => Pretty (ISpace v) where
  pretty (Shape s) = pretty s
  pretty (Dim d) = pretty d

ispaceToShape :: ISpace v -> Shape v
ispaceToShape (Dim d) = ShapeDim d
ispaceToShape (Shape s) = s

mapISpace :: (Dim v -> a) -> (Shape v -> a) -> ISpace v -> a
mapISpace f_dim f_shape ispace =
  case ispace of
    Dim d -> f_dim d
    Shape s -> f_shape s

fromDim :: ISpace v -> Maybe (Dim v)
fromDim (Dim d) = pure d
fromDim _ = Nothing

isDim :: ISpace v -> Bool
isDim = isJust . fromDim

fromShape :: ISpace v -> Maybe (Shape v)
fromShape (Shape s) = pure s
fromShape _ = Nothing

isShape :: ISpace v -> Bool
isShape = isJust . fromShape

-- | An ispace parameter, which is either a shape parameter or a dimension
-- parameter.
data ISpaceParam v
  = -- | A shape variable.
    ShapeParam v
  | -- | A dimension variable.
    DimParam v
  deriving (Show, Eq, Ord, Functor)

instance (Show v, Pretty v) => Pretty (ISpaceParam v) where
  pretty (ShapeParam v) = "@" <> pretty v
  pretty (DimParam v) = "$" <> pretty v

-- | Extract the variable out of an 'ISpaceParam'.
unISpaceParam :: ISpaceParam v -> v
unISpaceParam (ShapeParam v) = v
unISpaceParam (DimParam v) = v

-- | Coerce a shape into a dimension.
coerceToDim :: Shape v -> Maybe (Dim v)
coerceToDim (ShapeVar v) = pure $ DimVar v
coerceToDim (ShapeDim d) = pure d
coerceToDim _ = Nothing

-- | Basic dimension normalization; collects the numeric literals of a sum or
-- product into a single constant at the front and sorts the remaining terms.
normDim :: (Ord v) => Dim v -> Dim v
normDim (DimN n) = DimN n
normDim (DimVar v) = DimVar v
normDim (Sub ds) = Sub $ map normDim ds
normDim (Add ds) = normOp Add fromAdd (+) 0 ds
  where
    fromAdd (Add xs) = Just xs
    fromAdd _ = Nothing
normDim (Mul ds) = normOp Mul fromMul (*) 1 ds
  where
    fromMul (Mul xs) = Just xs
    fromMul _ = Nothing

-- | Normalize an operator that's commutative and associative. It flattens
-- nested occurences , folds numeric literals, and returns the constant followed
-- by the other terms in sorted order.
normOp ::
  (Ord v) =>
  ([Dim v] -> Dim v) ->
  (Dim v -> Maybe [Dim v]) ->
  (Int -> Int -> Int) ->
  Int ->
  [Dim v] ->
  Dim v
normOp mk flat op ident ds =
  case vars of
    [] -> DimN c
    _ -> mk (DimN c : sort vars)
  where
    terms = concatMap (\d -> fromMaybe [d] (flat $ normDim d)) ds
    (c, vars) = foldr step (ident, []) terms
    step (DimN n) = first (op n)
    step d = second (d :)

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

-- | Peel off the outermost dimension of a shape.
peelShape :: (Ord v) => Shape v -> Shape v
peelShape = peelShape' . normShape
  where
    peelShape' (Concat (_ : ss)) = Concat ss
    peelShape' _ = mempty

-- | Turns n list of integer dimensions into a 'Shape'.
intsToShape :: [Int] -> Shape v
intsToShape = Concat . map (ShapeDim . DimN)

-- | Evaluate a 'Dim' to an integer; resolves variables with the passed
-- resolving function.
dimToInt :: (Applicative f) => (v -> f Int) -> Dim v -> f Int
dimToInt resolve = dimToInt'
  where
    dimToInt' (DimVar v) = resolve v
    dimToInt' (DimN n) = pure n
    dimToInt' (Add ds) = sum <$> traverse dimToInt' ds
    dimToInt' (Mul ds) = product <$> traverse dimToInt' ds
    dimToInt' (Sub []) = pure 0
    dimToInt' (Sub [d]) = negate <$> dimToInt' d
    dimToInt' (Sub (d : ds)) =
      (\x xs -> x - sum xs) <$> dimToInt' d <*> traverse dimToInt' ds

-- | Evaluate a 'Shape' to a list of integers.
shapeToInts ::
  (Applicative f) => (Dim v -> f Int) -> (v -> f [Int]) -> Shape v -> f [Int]
shapeToInts dim resolveShape = shapeToInts'
  where
    shapeToInts' (ShapeVar v) = resolveShape v
    shapeToInts' (ShapeDim d) = pure <$> dim d
    shapeToInts' (Concat ss) = concat <$> traverse shapeToInts' ss
