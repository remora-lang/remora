{-# LANGUAGE UndecidableInstances #-}

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

instance (IsString s) => SExpable (NoInfo a) s where
  toSExp _ = "no-info"

instance (IsString s, SExpable a s) => SExpable (Info a) s where
  toSExp (Info a) =
    SList
      [ "info",
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

instance
  ( IsString s,
    SExpable v s,
    SExpable (f v) s,
    SExpable (f (AtomType f v)) s,
    SExpable (f (ArrayType f v)) s,
    SExpable (f (ArrayType f v, Shape v)) s
  ) =>
  SExpable (Atom f v) s
  where
  toSExp (Base b t pos) =
    SList
      [ "base",
        toSExp b,
        toSExp t,
        toSExp pos
      ]
  toSExp (Lambda params e t pos) =
    SList
      [ "fn",
        toSExp params,
        toSExp e,
        toSExp t,
        toSExp pos
      ]
  toSExp (TLambda tvars e t pos) =
    SList
      [ "t-fn",
        toSExp tvars,
        toSExp e,
        toSExp t,
        toSExp pos
      ]
  toSExp (ILambda ivars e t pos) =
    SList
      [ "t-fn",
        toSExp ivars,
        toSExp e,
        toSExp t,
        toSExp pos
      ]
  toSExp (Box shapes e t pos) =
    SList
      [ "box",
        toSExp shapes,
        toSExp e,
        toSExp t,
        toSExp pos
      ]

ascription :: (IsString s, SExpable a s) => Maybe a -> SExp s
ascription Nothing = "no-ascription"
ascription (Just a) =
  SList
    [ "ascription",
      toSExp a
    ]

instance
  ( IsString s,
    SExpable v s,
    SExpable (f v) s,
    SExpable (f (AtomType f v)) s,
    SExpable (f (ArrayType f v)) s,
    SExpable (f (ArrayType f v, Shape v)) s
  ) =>
  SExpable (Bind f v) s
  where
  toSExp (BindVal v mt e pos) =
    SList
      [ "bind-val",
        toSExp v,
        ascription mt,
        toSExp e,
        toSExp pos
      ]
  toSExp (BindFun f params mt body pos) =
    SList
      [ "bind-fun",
        toSExp f,
        toSExp params,
        ascription mt,
        toSExp body,
        toSExp pos
      ]
  toSExp (BindTFun f params mt body pos) =
    SList
      [ "bind-fun",
        toSExp f,
        toSExp params,
        ascription mt,
        toSExp body,
        toSExp pos
      ]
  toSExp (BindIFun f params mt body pos) =
    SList
      [ "bind-fun",
        toSExp f,
        toSExp params,
        ascription mt,
        toSExp body,
        toSExp pos
      ]
  toSExp (BindType v t pos) =
    SList
      [ "bind-type",
        toSExp v,
        toSExp t,
        toSExp pos
      ]
  toSExp (BindExtent v extent pos) =
    SList
      [ "bind-extent",
        toSExp v,
        toSExp extent,
        toSExp pos
      ]

instance
  ( IsString s,
    SExpable v s,
    SExpable (f v) s,
    SExpable (f (AtomType f v)) s,
    SExpable (f (ArrayType f v)) s,
    SExpable (f (ArrayType f v, Shape v)) s
  ) =>
  SExpable (Exp f v) s
  where
  toSExp (Var v t pos) =
    SList
      [ "var",
        toSExp v,
        toSExp t,
        toSExp pos
      ]
  toSExp (Array shape as t pos) =
    SList
      [ "array",
        SList $ "shape-lit" : map toSExp shape,
        toSExp as,
        toSExp t,
        toSExp pos
      ]
  toSExp (EmptyArray shape t t' pos) =
    SList
      [ "empty-array",
        SList $ "shape-lit" : map toSExp shape,
        toSExp t,
        toSExp t',
        toSExp pos
      ]
  toSExp (Frame shape es t pos) =
    SList
      [ "frame",
        SList $ "shape-lit" : map toSExp shape,
        toSExp es,
        toSExp t,
        toSExp pos
      ]
  toSExp (EmptyFrame shape t t' pos) =
    SList
      [ "empty-frame",
        toSExp shape,
        SList $ "shape-lit" : map toSExp shape,
        toSExp t,
        toSExp t',
        toSExp pos
      ]
  toSExp (App f es t pos) =
    SList
      [ "app",
        toSExp f,
        toSExp es,
        toSExp t,
        toSExp pos
      ]
  toSExp (TApp e ts t pos) =
    SList
      [ "t-app",
        toSExp e,
        toSExp ts,
        toSExp t,
        toSExp pos
      ]
  toSExp (IApp e is t pos) =
    SList
      [ "i-app",
        toSExp e,
        toSExp is,
        toSExp t,
        toSExp pos
      ]
  toSExp (Unbox vs v_e x_e e t pos) =
    SList
      [ "unbox",
        toSExp vs,
        toSExp v_e,
        toSExp x_e,
        toSExp e,
        toSExp t,
        toSExp pos
      ]
  toSExp (Let bs e t pos) =
    SList
      [ "let",
        toSExp bs,
        toSExp e,
        toSExp t,
        toSExp pos
      ]

instance (IsString s, SExpable v s) => SExpable (TypeParam v) s where
  toSExp (AtomTypeParam v) =
    SList
      [ "atom-tvar",
        toSExp v
      ]
  toSExp (ArrayTypeParam v) =
    SList
      [ "array-tvar",
        toSExp v
      ]

instance (IsString s, SExpable v s) => SExpable (ExtentParam v) s where
  toSExp (ShapeParam v) =
    SList
      [ "shape-ivar",
        toSExp v
      ]
  toSExp (DimParam v) =
    SList
      [ "dim-ivar",
        toSExp v
      ]

instance (IsString s, SExpable v s) => SExpable (Extent v) s where
  toSExp (Dim v) =
    SList
      [ "extent-dim",
        toSExp v
      ]
  toSExp (Shape v) =
    SList
      [ "extent-shape",
        toSExp v
      ]

instance
  ( IsString s,
    SExpable v s,
    SExpable (f v) s
  ) =>
  SExpable (ArrayType f v) s
  where
  toSExp (A t shape) =
    SList
      [ "A",
        toSExp t,
        toSExp shape
      ]
  toSExp (ArrayTypeVar t et s) =
    SList
      [ "ArrayTypeVar",
        toSExp t,
        toSExp et,
        toSExp s
      ]

instance
  ( IsString s,
    SExpable v s,
    SExpable (f v) s
  ) =>
  SExpable (AtomType f v) s
  where
  toSExp (AtomTypeVar v) = toSExp v
  toSExp Bool = "Bool"
  toSExp Int = "Int"
  toSExp Float = "Float"
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
  toSExp (Pi vs t) =
    SList
      [ "prod",
        toSExp vs,
        toSExp t
      ]
  toSExp (Sigma vs t) =
    SList
      [ "exists",
        toSExp vs,
        toSExp t
      ]

instance
  ( IsString s,
    SExpable v s,
    SExpable (f v) s
  ) =>
  SExpable (Type f v) s
  where
  toSExp (ArrayType t) =
    SList ["array-type", toSExp t]
  toSExp (AtomType t) =
    SList ["scalar-type", toSExp t]

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
