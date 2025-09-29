module Shape where

import Data.Bifunctor
import Data.SBV
import Prettyprinter

data Dim v
  = DimVar v
  | Dim Int
  | Add [Dim v]

deriving instance (Show v) => Show (Dim v)

deriving instance (Eq v) => Eq (Dim v)

instance (Show v, Pretty v) => Pretty (Dim v) where
  pretty (DimVar v) = pretty v
  pretty (Dim d) = pretty d
  pretty (Add ds) = parens $ hsep ("+" : map pretty ds)

data Shape v
  = ShapeVar v
  | ShapeDim (Dim v)
  | Concat [Shape v]

deriving instance (Show v) => Show (Shape v)

deriving instance (Eq v) => Eq (Shape v)

instance Semigroup (Shape v) where
  s <> t = Concat [s, t]

instance Monoid (Shape v) where
  mempty = Concat []

instance (Show v, Pretty v) => Pretty (Shape v) where
  pretty (ShapeVar v) = pretty v
  pretty (ShapeDim d) = pretty d
  -- pretty (Shape is) = parens $ hsep ("shape" : map pretty is)
  pretty (Concat []) = "•"
  pretty (Concat is) = parens $ hsep ("++" : map pretty is)

sepDim :: Shape v -> Either (Dim v) (Shape v)
sepDim (ShapeDim d) = Left d
sepDim (Concat [s]) = sepDim s
sepDim s = Right s

normDim :: (Eq v) => Dim v -> Dim v
normDim (Dim n) = Dim n
normDim (DimVar v) = DimVar v
normDim (Add ds) =
  case (d, vars) of
    (_, []) -> Dim d
    _ -> Add (Dim d : vars)
  where
    (d, vars) =
      foldr discriminate (0, []) $
        flip concatMap ds $ \d ->
          case normDim d of
            Add ds' -> ds'
            d' -> pure d'
    discriminate d =
      case d of
        Dim n -> first (+ n)
        DimVar v -> second (DimVar v :)
        _ -> error ""

normShape :: (Eq v) => Shape v -> Shape v
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

decrementDim :: Dim v -> Dim v
decrementDim (Dim 0) = error "decrementDim"
decrementDim (Dim n) = Dim $ n - 1
decrementDim d = Add [Dim (-1), d]

-- peels the first dim off a shape
peel :: (Eq v) => Shape v -> Maybe (Dim v, Shape v)
peel = peel' . normShape
  where
    peel' (ShapeDim d) = Just (d, mempty)
    peel' (Concat (s : ss)) = peel s
    peel' _ = Nothing
