module Syntax where

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
  = BoolVal Bool
  | IntegerVal Int
  | Lambda [(v, f (Type v))] (Exp f v)
  | TLambda [(v, Kind)] (Exp f v)
  | ILambda [(v, Sort)] (Exp f v)
  | Box [Idx v] (Exp f v) (f (Type v))

data Type v
  = TVar v
  | Bool
  | Integer

data Exp f v
  = Var v
  | Array [Int] [Atom f v]
  | EmptyArray [Int] (f (Type v))
  | Frame [Int] [Exp f v]
  | EmptyFrame [Int] (f (Type v))
  | App [Exp f v]
  | TApp (Exp f v) [Type v]
  | IApp (Exp f v) [Idx v]
  | Unbox [v] (Exp f v) (Exp f v)
