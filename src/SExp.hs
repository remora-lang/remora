module SExp where

import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter
import Syntax
import Text.Megaparsec.Pos
import Util

data SExp a
  = SList [SExp a]
  | SAtom a
  deriving (Show, Eq)

instance (IsString a) => IsString (SExp a) where
  fromString = SAtom . fromString

instance (Pretty a) => Pretty (SExp a) where
  pretty (SList sexps) = "(" <> hsep (map pretty sexps) <> ")"
  pretty (SAtom a) = pretty a

class SExpable a b where
  toSExp :: a -> SExp b

instance (SExpable a c, SExpable b c) => SExpable (a, b) c where
  toSExp (a, b) =
    SList [toSExp a, toSExp b]

instance (SExpable a c) => SExpable [a] c where
  toSExp = SList . map toSExp

instance {-# OVERLAPS #-} SExpable FilePath Text where
  toSExp = SAtom . T.pack

instance SExpable Int Text where
  toSExp = SAtom . T.pack . show

instance SExpable Pos Text where
  toSExp p =
    SList ["pos", toSExp $ unPos p]

instance SExpable Text Text where
  toSExp = SAtom

instance SExpable SourcePos Text where
  toSExp pos =
    SList
      [ "source-pos",
        SList ["source-file", toSExp $ sourceName pos],
        SList ["source-line", toSExp $ sourceLine pos],
        SList ["source-column", toSExp $ sourceColumn pos]
      ]

instance SExpable Base Text where
  toSExp (BoolVal b) =
    SList
      [ "bool",
        if b then "#t" else "#f"
      ]
  toSExp (IntVal x) =
    SList
      [ "int",
        toSExp $ prettyText x
      ]
  toSExp (FloatVal x) =
    SList
      [ "float",
        toSExp $ prettyText x
      ]

instance SExpable (Atom Unchecked Text) Text where
  toSExp (Base b _ pos) =
    SList
      [ "base",
        toSExp b,
        toSExp pos
      ]
  toSExp (Lambda params e _ pos) =
    SList
      [ "fn",
        toSExp params,
        toSExp e,
        toSExp pos
      ]
  toSExp (TLambda tvars e _ pos) =
    SList
      [ "t-fn",
        toSExp tvars,
        toSExp e,
        toSExp pos
      ]
  toSExp (ILambda ivars e _ pos) =
    SList
      [ "t-fn",
        toSExp ivars,
        toSExp e,
        toSExp pos
      ]
  toSExp (Box shapes e _ pos) =
    SList
      [ "box",
        toSExp shapes,
        toSExp e,
        toSExp pos
      ]

instance SExpable (Exp Unchecked Text) Text where
  toSExp (Var v _ pos) =
    SList
      [ "var",
        toSExp v,
        toSExp pos
      ]
  toSExp (Array shape as _ pos) =
    SList
      [ "array",
        SList $ "shape-lit" : map toSExp shape,
        toSExp as,
        toSExp pos
      ]
  toSExp (EmptyArray shape t _ pos) =
    SList
      [ "empty-array",
        SList $ "shape-lit" : map toSExp shape,
        toSExp t,
        toSExp pos
      ]
  toSExp (Frame shape es _ pos) =
    SList
      [ "frame",
        SList $ "shape-lit" : map toSExp shape,
        toSExp es,
        toSExp pos
      ]
  toSExp (EmptyFrame shape t _ pos) =
    SList
      [ "empty-frame",
        toSExp shape,
        SList $ "shape-lit" : map toSExp shape,
        toSExp t,
        toSExp pos
      ]
  toSExp (App f es _ pos) =
    SList
      [ "app",
        toSExp f,
        toSExp es,
        toSExp pos
      ]
  toSExp (TApp e ts _ pos) =
    SList
      [ "t-app",
        toSExp e,
        toSExp ts,
        toSExp pos
      ]
  toSExp (Unbox vs v_e x_e e _ pos) =
    SList
      [ "unbox",
        toSExp vs,
        toSExp v_e,
        toSExp x_e,
        toSExp e,
        toSExp pos
      ]

instance SExpable (TVar Text) Text where
  toSExp (AtomTVar v) =
    SList
      [ "atom-tvar",
        toSExp v
      ]
  toSExp (ArrayTVar v) =
    SList
      [ "array-tvar",
        toSExp v
      ]

instance SExpable (IVar Text) Text where
  toSExp (SVar v) =
    SList
      [ "shape-ivar",
        toSExp v
      ]
  toSExp (DVar v) =
    SList
      [ "dim-ivar",
        toSExp v
      ]

instance SExpable (Type Text) Text where
  toSExp (TVar v) = toSExp v
  toSExp Bool = "Bool"
  toSExp Int = "Int"
  toSExp Float = "Float"
  toSExp (TArr t shape) =
    SList
      [ "A",
        toSExp t,
        toSExp shape
      ]
  toSExp (ts :-> t) =
    SList
      [ "->",
        toSExp ts,
        toSExp t
      ]
  toSExp (Forall vs t) =
    SList
      [ "forall",
        toSExp vs,
        toSExp t
      ]
  toSExp (DProd vs t) =
    SList
      [ "prod",
        toSExp vs,
        toSExp t
      ]
  toSExp (DSum vs t) =
    SList
      [ "exists",
        toSExp vs,
        toSExp t
      ]

instance SExpable (Dim Text) Text where
  toSExp (DimVar v) =
    SList
      [ "dim-ivar",
        toSExp v
      ]
  toSExp (Dim n) =
    SList
      [ "dim",
        toSExp n
      ]
  toSExp (Add ds) =
    SList $ "dim-+" : map toSExp ds

instance SExpable (Shape Text) Text where
  toSExp (ShapeVar v) =
    SList
      [ "shape-ivar",
        toSExp v
      ]
  toSExp (ShapeDim d) =
    SList ["shape-dim", toSExp d]
  toSExp (Concat ss) =
    SList $ "shape-++" : map toSExp ss
