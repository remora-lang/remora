module Shape where

import Prettyprinter

data Dim v
  = DimVar v
  | Dim Int

deriving instance (Show v) => Show (Dim v)

deriving instance (Eq v) => Eq (Dim v)

instance (Show v, Pretty v) => Pretty (Dim v) where
  pretty (DimVar v) = pretty v
  pretty (Dim d) = pretty d

data Shape v
  = ShapeVar v
  | Shape [Dim v]
  | Add [Dim v]
  | Concat [Shape v]

deriving instance (Show v) => Show (Shape v)

deriving instance (Eq v) => Eq (Shape v)

instance Semigroup (Shape v) where
  s <> t = Concat [s, t]

instance Monoid (Shape v) where
  mempty = Shape []

instance (Show v, Pretty v) => Pretty (Shape v) where
  pretty (ShapeVar v) = pretty v
  pretty (Shape is) = parens $ hsep ("shape" : map pretty is)
  pretty (Add is) = parens $ hsep ("+" : map pretty is)
  pretty (Concat is) = parens $ hsep ("++" : map pretty is)

-- peels the first shape off a shape
peel :: Shape v -> Maybe (Dim v, Shape v)
peel (Shape (d : ds)) = Just (d, Shape ds)
peel (Concat (s : ss)) = peel s
peel _ = Nothing
