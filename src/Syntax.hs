module Syntax where

import Text.Megaparsec.Pos (SourcePos)

data Idx v
  = IdxVar v
  | Dim Int
  | Shape [Idx v]
  | (:+) (Idx v) (Idx v)
  | (:++) (Idx v) (Idx v)

data Kind
  = KindArray
  | KindAtom
  deriving (Show, Eq, Ord)

data Sort
  = SortShape
  | SortDim
  deriving (Show, Eq, Ord)

data Atom f v
  = BoolVal Bool SourcePos
  | IntegerVal Int SourcePos
  | FloatVal Float SourcePos
  | Lambda [(v, f (Type v))] (Exp f v) SourcePos
  | TLambda [(v, Kind)] (Exp f v) SourcePos
  | ILambda [(v, Sort)] (Exp f v) SourcePos
  | Box [Idx v] (Exp f v) (f (Type v)) SourcePos

data Type v
  = TVar v
  | Bool
  | Integer

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
