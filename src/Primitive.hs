module Primitive
  ( UnOp (..),
    unOps,
    BinOp (..),
    binOps,
  )
where

import Data.List.NonEmpty qualified as NE
import Prettyprinter
import Syntax (ArrayType, AtomType (..), arrowType, mkScalarArrayType)

data UnOp
  = Sqrt
  | FSqrt
  | BitNot
  | Popc
  | Not
  | IntToFloat
  | IntToBool
  | BoolToInt
  | BoolToFloat
  | Truncate
  | Round
  | Ceiling
  | Floor
  deriving (Eq, Ord, Show)

instance Pretty UnOp where
  pretty Sqrt = "sqrt"
  pretty FSqrt = "f.sqrt"
  pretty BitNot = "bit-not"
  pretty Popc = "popc"
  pretty Not = "not"
  pretty IntToFloat = "i->f"
  pretty IntToBool = "i->bool"
  pretty BoolToInt = "bool->i"
  pretty BoolToFloat = "bool->f"
  pretty Truncate = "truncate"
  pretty Round = "round"
  pretty Ceiling = "ceiling"
  pretty Floor = "floor"

unOps :: [(UnOp, ArrayType v)]
unOps =
  [ (Sqrt, unT Float Float),
    (FSqrt, unT Float Float),
    (BitNot, unT Int Int),
    (Popc, unT Int Int),
    (Not, unT Bool Bool),
    (IntToFloat, unT Int Float),
    (IntToBool, unT Int Bool),
    (BoolToInt, unT Bool Int),
    (BoolToFloat, unT Bool Float),
    (Truncate, unT Float Int),
    (Round, unT Float Int),
    (Ceiling, unT Float Int),
    (Floor, unT Float Int)
  ]
  where
    unT a r = mkScalarArrayType $ mkScalarArrayType a :-> mkScalarArrayType r

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  | Mod
  | Max
  | Min
  | BitAnd
  | BitOr
  | BitXor
  | Shl
  | Shr
  | FAdd
  | FSub
  | FMul
  | FDiv
  | FPow
  | FMax
  | FMin
  | Eq
  | Neq
  | Lt
  | Gt
  | Le
  | Ge
  | FEq
  | FNeq
  | FLt
  | FGt
  | FLe
  | FGe
  | And
  | Or
  | BEq
  | BNeq
  deriving (Eq, Ord, Show)

instance Pretty BinOp where
  pretty Add = "+"
  pretty Sub = "-"
  pretty Mul = "*"
  pretty Div = "/"
  pretty Pow = "^"
  pretty Mod = "mod"
  pretty Max = "max"
  pretty Min = "min"
  pretty BitAnd = "bit-and"
  pretty BitOr = "bit-or"
  pretty BitXor = "bit-xor"
  pretty Shl = "shl"
  pretty Shr = "shr"
  pretty FAdd = "f.+"
  pretty FSub = "f.-"
  pretty FMul = "f.*"
  pretty FDiv = "f./"
  pretty FPow = "f.^"
  pretty FMax = "f.max"
  pretty FMin = "f.min"
  pretty Eq = "=="
  pretty Neq = "!="
  pretty Lt = "<"
  pretty Gt = ">"
  pretty Le = "<="
  pretty Ge = ">="
  pretty FEq = "f.=="
  pretty FNeq = "f.!="
  pretty FLt = "f.<"
  pretty FGt = "f.>"
  pretty FLe = "f.<="
  pretty FGe = "f.>="
  pretty And = "and"
  pretty Or = "or"
  pretty BEq = "bool.=="
  pretty BNeq = "bool.!="

binOps :: [(BinOp, ArrayType v)]
binOps =
  [ (Add, binT Int Int),
    (Sub, binT Int Int),
    (Mul, binT Int Int),
    (Div, binT Int Int),
    (Pow, binT Int Int),
    (Mod, binT Int Int),
    (Max, binT Int Int),
    (Min, binT Int Int),
    (BitAnd, binT Int Int),
    (BitOr, binT Int Int),
    (BitXor, binT Int Int),
    (Shl, binT Int Int),
    (Shr, binT Int Int),
    (FAdd, binT Float Float),
    (FSub, binT Float Float),
    (FMul, binT Float Float),
    (FDiv, binT Float Float),
    (FPow, binT Float Float),
    (FMax, binT Float Float),
    (FMin, binT Float Float),
    (Eq, binT Int Bool),
    (Neq, binT Int Bool),
    (Lt, binT Int Bool),
    (Gt, binT Int Bool),
    (Le, binT Int Bool),
    (Ge, binT Int Bool),
    (FEq, binT Float Bool),
    (FNeq, binT Float Bool),
    (FLt, binT Float Bool),
    (FGt, binT Float Bool),
    (FLe, binT Float Bool),
    (FGe, binT Float Bool),
    (And, binT Bool Bool),
    (Or, binT Bool Bool),
    (BEq, binT Bool Bool),
    (BNeq, binT Bool Bool)
  ]
  where
    binT a r =
      mkScalarArrayType $
        NE.fromList [mkScalarArrayType a, mkScalarArrayType a] `arrowType` mkScalarArrayType r
