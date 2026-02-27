module Primitive
  ( UnOp (..),
    unOps,
    BinOp (..),
    binOps,
  )
where

import Data.Bifunctor
import Prettyprinter
import Syntax (ArrayType, AtomType (..), mkScalarArrayType)

data UnOp
  = Sqrt
  deriving (Eq, Ord, Show)

instance Pretty UnOp where
  pretty Sqrt = "f.sqrt"

unOps :: [(UnOp, ArrayType v)]
unOps =
  map (second mkUnOpType) $
    [(Sqrt, Float)]
  where
    mkUnOpType :: AtomType v -> ArrayType v
    mkUnOpType t =
      mkScalarArrayType $ [t'] :-> t'
      where
        t' = mkScalarArrayType t

data BinOp
  = Add
  | FAdd
  | Sub
  | FSub
  | Mul
  | FMul
  | Div
  | FDiv
  | Pow
  | FPow
  deriving (Eq, Ord, Show)

instance Pretty BinOp where
  pretty Add = "+"
  pretty FAdd = "f.+"
  pretty Sub = "-"
  pretty FSub = "f.-"
  pretty Mul = "*"
  pretty FMul = "f.*"
  pretty Div = "/"
  pretty FDiv = "f./"
  pretty Pow = "^"
  pretty FPow = "f.^"

binOps :: [(BinOp, ArrayType v)]
binOps =
  map (second mkBinOpType) $
    [ (Add, Int),
      (FAdd, Float),
      (Sub, Int),
      (FSub, Float),
      (Mul, Int),
      (FMul, Float),
      (Div, Int),
      (FDiv, Float),
      (Pow, Int),
      (FPow, Float)
    ]
  where
    mkBinOpType :: AtomType v -> ArrayType v
    mkBinOpType t =
      mkScalarArrayType $ [t', t'] :-> t'
      where
        t' = mkScalarArrayType t
