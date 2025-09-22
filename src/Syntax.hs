module Syntax where

import Prettyprinter
import Text.Megaparsec.Pos (SourcePos)

data Idx v
  = IdxVar v SourcePos
  | Dim Int SourcePos
  | Shape [Idx v] SourcePos
  | Add [Idx v] SourcePos
  | Concat [Idx v] SourcePos

deriving instance (Show v) => Show (Idx v)

instance (Show v, Pretty v) => Pretty (Idx v) where
  pretty (IdxVar v _) = pretty v
  pretty (Dim d _) = pretty d
  pretty (Shape is _) = parens $ hsep ("shape" : map pretty is)
  pretty (Add is _) = parens $ hsep ("+" : map pretty is)
  pretty (Concat is _) = parens $ hsep ("++" : map pretty is)

data Kind
  = KindArray
  | KindAtom
  deriving (Show, Eq, Ord)

instance Pretty Kind where
  pretty KindArray = "Array"
  pretty KindAtom = "Atom"

data Sort
  = SortShape
  | SortDim
  deriving (Show, Eq, Ord)

instance Pretty Sort where
  pretty SortShape = "Shape"
  pretty SortDim = "Dim"

data Base
  = BoolVal Bool
  | IntVal Int
  | FloatVal Float
  deriving (Show, Eq, Ord)

instance Pretty Base where
  pretty (BoolVal True) = "#t"
  pretty (BoolVal False) = "#f"
  pretty (IntVal i) = pretty i
  pretty (FloatVal f) = pretty f

data Atom v
  = Base Base SourcePos
  | Lambda [(v, Type v)] (Exp v) SourcePos
  | TLambda [(v, Kind)] (Exp v) SourcePos
  | ILambda [(v, Sort)] (Exp v) SourcePos
  | Box [Idx v] (Exp v) (Type v) SourcePos

deriving instance (Show v) => Show (Atom v)

instance (Show v, Pretty v) => Pretty (Atom v) where
  pretty (Base b _) = pretty b
  pretty (Lambda args e _) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, t) ->
                    tupled $ [pretty v, pretty t]
                )
                args
     in parens $ "λ" <+> pArgs <+> pretty e
  pretty (TLambda args e _) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, k) ->
                    tupled $ [pretty v, pretty k]
                )
                args
     in parens $ "Tλ" <+> pArgs <+> pretty e
  pretty (ILambda args e _) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, s) ->
                    tupled $ [pretty v, pretty s]
                )
                args
     in parens $ "Iλ" <+> pArgs <+> pretty e
  pretty (Box is e t _) =
    parens $ "box" <+> hsep (map pretty is) <+> pretty e <+> pretty t

data Type v
  = TVar v
  | Bool
  | Int
  | Float
  | (:->) [Type v] (Type v)
  | Forall [(v, Kind)] (Type v)
  | DProd [(v, Sort)] (Type v)
  | DSum [(v, Sort)] (Type v)

deriving instance (Show v) => Show (Type v)

instance (Show v, Pretty v) => Pretty (Type v) where
  pretty (TVar v) = pretty v
  pretty Bool = "Bool"
  pretty Int = "Int"
  pretty Float = "Float"
  pretty (ts :-> t) =
    parens $ "->" <+> parens (hsep (map pretty ts)) <+> pretty t
  pretty (Forall xs t) =
    parens $
      "∀"
        <+> parens (hsep (map (\(x, k) -> pretty x <+> pretty k) xs))
        <+> pretty t
  pretty (DProd xs t) =
    parens $
      "Π"
        <+> parens (hsep (map (\(x, s) -> pretty x <+> pretty s) xs))
        <+> pretty t
  pretty (DSum xs t) =
    parens $
      "Σ"
        <+> parens (hsep (map (\(x, s) -> pretty x <+> pretty s) xs))
        <+> pretty t

data Exp v
  = Var v SourcePos
  | Array [Int] [Atom v] SourcePos
  | EmptyArray [Int] (Type v) SourcePos
  | Frame [Int] [Exp v] SourcePos
  | EmptyFrame [Int] (Type v) SourcePos
  | App [Exp v] SourcePos
  | TApp (Exp v) [Type v] SourcePos
  | IApp (Exp v) [Idx v] SourcePos
  | Unbox [v] (Exp v) (Exp v) SourcePos
  | Atom (Atom v)

deriving instance (Show v) => Show (Exp v)

instance (Show v, Pretty v) => Pretty (Exp v) where
  pretty (Var v _) = pretty v
  pretty (Array shape as _) =
    group $
      parens $
        "Array"
          <+> (parens $ hsep (map pretty shape))
          <+> (group $ encloseSep "[" "]" ("," <> line) (map pretty as))
  pretty (Frame shape es _) =
    group $
      parens $
        "Array"
          <+> (parens $ hsep (map pretty shape))
          <+> (group $ encloseSep "[" "]" ("," <> line) (map pretty es))
  pretty (App es _) =
    parens $ hsep $ map pretty es
  pretty (TApp e ts _) =
    parens $ "t-app" <+> pretty e <+> hsep (map pretty ts)
  pretty (IApp e is _) =
    parens $ "i-app" <+> pretty e <+> hsep (map pretty is)
  pretty (Unbox vs e b _) =
    parens $ "unbox" <+> (parens (hsep (map pretty vs ++ [pretty e]))) <+> pretty b
  pretty (Atom a) = pretty a
