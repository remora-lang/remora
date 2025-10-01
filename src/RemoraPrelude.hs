module RemoraPrelude (prelude, Prelude, PreludeDef (..)) where

import Control.Monad
import Data.Text (Text)
import Interpreter.Value
import Syntax
import VName

type Prelude v m = [PreludeDef v m]

data PreludeDef v m
  = PreludeVal v (Type v) (Val m)
  | PreludeType v Kind

prelude :: (Monad m) => Prelude Text m
prelude =
  [ PreludeVal
      "head"
      ( Forall
          [("t", KindAtom)]
          ( DProd
              [("d", SortDim), ("s", SortShape)]
              ( [ TArr
                    (TVar "t")
                    ( Concat
                        [ ShapeDim (Add [Dim 1, DimVar "d"]),
                          ShapeVar "s"
                        ]
                    )
                ]
                  :-> TArr (TVar "t") (ShapeVar "s")
              )
          )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[_d, _s] ->
            pure $ ValFun $ \[ValArray (d : ds) vs] ->
              pure $ ValArray (d - 1 : ds) $ init vs
      ),
    PreludeVal
      "tail"
      ( Forall
          [("t", KindAtom)]
          ( DProd
              [("d", SortDim), ("s", SortShape)]
              ( [ TArr
                    (TVar "t")
                    ( Concat
                        [ ShapeDim (Add [Dim 1, DimVar "d"]),
                          ShapeVar "s"
                        ]
                    )
                ]
                  :-> TArr (TVar "t") (ShapeVar "s")
              )
          )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[_d, _s] ->
            pure $ ValFun $ \[ValArray (d : ds) vs] ->
              pure $ ValArray (d - 1 : ds) $ tail vs
      ),
    PreludeVal
      "length"
      ( Forall
          [("t", KindAtom)]
          ( DProd
              [("d", SortDim), ("s", SortShape)]
              ( [TArr (TVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])]
                  :-> TArr Int mempty
              )
          )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[_d, _s] ->
            pure $ ValFun $ \[ValArray (d : _) _] ->
              pure $ ValBase $ IntVal $ d
      ),
    PreludeVal
      "append"
      ( Forall
          [("t", KindAtom)]
          ( DProd
              [("m", SortDim), ("n", SortDim), ("s", SortShape)]
              ( [ TArr (TVar "t") (Concat [ShapeDim $ DimVar "m", ShapeVar "s"]),
                  TArr (TVar "t") (Concat [ShapeDim $ DimVar "n", ShapeVar "s"])
                ]
                  :-> TArr
                    (TVar "t")
                    ( Concat
                        [ ShapeDim $ DimVar "m",
                          ShapeDim $ DimVar "n",
                          ShapeVar "s"
                        ]
                    )
              )
          )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[[m], [n], s] ->
            pure $ ValFun $ \[ValArray _ xs, ValArray _ ys] ->
              pure $ ValArray (m + n : s) (xs ++ ys)
      ),
    PreludeVal
      "reverse"
      ( Forall
          [("t", KindAtom)]
          ( DProd
              [("d", SortDim), ("s", SortShape)]
              ( let arr_t =
                      TArr
                        (TVar "t")
                        ( Concat
                            [ShapeDim $ DimVar "d", ShapeVar "s"]
                        )
                 in [arr_t] :-> arr_t
              )
          )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[[_d], _s] ->
            pure $ ValFun $ \[ValArray shape xs] ->
              pure $ ValArray shape (reverse xs)
      ),
    PreludeVal
      "+"
      ([Int, Int] :-> Int)
      ( ValFun $ \[ValBase (IntVal x), ValBase (IntVal y)] ->
          pure $ ValBase $ IntVal $ x + y
      )
  ]
