module Syntax where

import Text.Megaparsec.Pos (SourcePos)

data Idx v
  = IdxVar v SourcePos
  | Dim Int SourcePos
  | Shape [Idx v] SourcePos
  | Add [Idx v] SourcePos
  | Concat [Idx v] SourcePos

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
  | IntVal Int SourcePos
  | FloatVal Float SourcePos
  | Lambda [(v, Maybe (Type v))] (Exp f v) SourcePos
  | TLambda [(v, Maybe Kind)] (Exp f v) SourcePos
  | ILambda [(v, Maybe Sort)] (Exp f v) SourcePos
  | Box [Idx v] (Exp f v) (Maybe (Type v)) SourcePos

data Type v
  = TVar v
  | Bool
  | Int
  | Float

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
