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

instance {-# OVERLAPS #-} (IsString s) => SExpable FilePath s where
  toSExp = SAtom . fromString

instance (IsString s) => SExpable Int s where
  toSExp = SAtom . fromString . show

instance (IsString s) => SExpable Pos s where
  toSExp = toSExp . unPos

instance (IsString s) => SExpable Text s where
  toSExp = SAtom . fromString . T.unpack

instance (IsString s) => SExpable SourcePos s where
  toSExp pos =
    SList
      [ "source-pos",
        SList ["file", toSExp $ sourceName pos],
        SList ["line", toSExp $ sourceLine pos],
        SList ["col", toSExp $ sourceColumn pos]
      ]

instance (IsString s) => SExpable (Unchecked a) s where
  toSExp _ = "unchecked"

instance (IsString s, SExpable a s) => SExpable (Typed a) s where
  toSExp (Typed a) =
    SList
      [ "type",
        toSExp a
      ]

instance (IsString s) => SExpable Base s where
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

instance (IsString s, SExpable v s, SExpable (f v) s) => SExpable (Atom f v) s where
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

instance (IsString s, SExpable v s, SExpable (f v) s) => SExpable (Exp f v) s where
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
  toSExp (IApp e is _ pos) =
    SList
      [ "i-app",
        toSExp e,
        toSExp is,
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

instance (IsString s, SExpable v s) => SExpable (TVar v) s where
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

instance (IsString s, SExpable v s) => SExpable (IVar v) s where
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

instance (IsString s, SExpable v s) => SExpable (Idx v) s where
  toSExp (Dim v) =
    SList
      [ "idx-dim",
        toSExp v
      ]
  toSExp (Shape v) =
    SList
      [ "idx-shape",
        toSExp v
      ]

instance (IsString s, SExpable v s) => SExpable (Type v) s where
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
  toSExp (Prod vs t) =
    SList
      [ "prod",
        toSExp vs,
        toSExp t
      ]
  toSExp (Exists vs t) =
    SList
      [ "exists",
        toSExp vs,
        toSExp t
      ]

instance (IsString s, SExpable v s) => SExpable (Dim v) s where
  toSExp (DimVar v) =
    SList
      [ "dim-ivar",
        toSExp v
      ]
  toSExp (DimN n) =
    SList
      [ "dim-n",
        toSExp n
      ]
  toSExp (Add ds) =
    SList $ "dim-+" : map toSExp ds

instance (IsString s, SExpable v s) => SExpable (Shape v) s where
  toSExp (ShapeVar v) =
    SList
      [ "shape-ivar",
        toSExp v
      ]
  toSExp (ShapeDim d) =
    SList ["shape-dim", toSExp d]
  toSExp (Concat ss) =
    SList $ "shape-++" : map toSExp ss
