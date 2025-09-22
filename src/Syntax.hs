module Syntax where

import Data.Proxy
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

data Atom f v
  = Base Base SourcePos
  | Lambda [(v, Maybe (Type v))] (Exp f v) SourcePos
  | TLambda [(v, Maybe Kind)] (Exp f v) SourcePos
  | ILambda [(v, Maybe Sort)] (Exp f v) SourcePos
  | Box [Idx v] (Exp f v) (Maybe (Type v)) SourcePos

deriving instance (Show v) => Show (Atom Proxy v)

instance (Show v, Pretty v) => Pretty (Atom Proxy v) where
  pretty (Base b _) = pretty b
  pretty (Lambda args e _) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, mt) ->
                    case mt of
                      Nothing -> pretty v
                      Just t -> tupled $ [pretty v, pretty t]
                )
                args
     in parens $ "λ" <+> pArgs <+> pretty e
  pretty (TLambda args e _) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, mk) ->
                    case mk of
                      Nothing -> pretty v
                      Just k -> tupled $ [pretty v, pretty k]
                )
                args
     in parens $ "Tλ" <+> pArgs <+> pretty e
  pretty (ILambda args e _) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, ms) ->
                    case ms of
                      Nothing -> pretty v
                      Just s -> tupled $ [pretty v, pretty s]
                )
                args
     in parens $ "Iλ" <+> pArgs <+> pretty e
  pretty (Box is e mt _) =
    parens $ "box" <+> hsep (map pretty is) <+> pretty e <+> maybe "" pretty mt

data Type v
  = TVar v
  | Bool
  | Int
  | Float

deriving instance (Show v) => Show (Type v)

instance (Show v, Pretty v) => Pretty (Type v) where
  pretty (TVar v) = pretty v
  pretty Bool = "Bool"
  pretty Int = "Int"
  pretty Float = "Float"

data Exp f v
  = Var v SourcePos
  | Array [Int] [Atom f v] SourcePos
  | EmptyArray [Int] (f (Type v)) SourcePos
  | Frame [Int] [Exp f v] SourcePos
  | EmptyFrame [Int] (f (Type v)) SourcePos
  | App [Exp f v] SourcePos
  | TApp (Exp f v) [Type v] SourcePos
  | IApp (Exp f v) [Idx v] SourcePos
  | Unbox [v] (Exp f v) (Exp f v) SourcePos
  | Atom (Atom f v)

deriving instance (Show v) => Show (Exp Proxy v)

instance (Show v, Pretty v) => Pretty (Exp Proxy v) where
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
