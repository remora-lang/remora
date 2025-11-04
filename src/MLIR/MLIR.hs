-- Haskell datatype that corresponds to the MLIR Remora dialect

module MLIR where

data MLIRExp
  = Linalg

class MLIRable a where
  toMLIR :: a -> 
