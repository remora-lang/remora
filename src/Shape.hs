module Shape where

import Prettyprinter

data Shape v
  = ShapeVar v
  | Dim Int
  | Shape [Shape v]
  | Add [Shape v]
  | Concat [Shape v]

deriving instance (Show v) => Show (Shape v)

deriving instance (Eq v) => Eq (Shape v)

instance Semigroup (Shape v) where
  s <> t = Concat [s, t]

instance Monoid (Shape v) where
  mempty = Shape []

instance (Show v, Pretty v) => Pretty (Shape v) where
  pretty (ShapeVar v) = pretty v
  pretty (Dim d) = pretty d
  pretty (Shape is) = parens $ hsep ("shape" : map pretty is)
  pretty (Add is) = parens $ hsep ("+" : map pretty is)
  pretty (Concat is) = parens $ hsep ("++" : map pretty is)

-- peels the first shape off a shape; assumes normalized as `Shape`
peel :: Shape v -> Maybe (Shape v, Shape v)
peel (Shape (s : ss)) = Just (s, Shape ss)
peel _ = Nothing
