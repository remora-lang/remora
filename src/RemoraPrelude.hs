module RemoraPrelude (prelude, Prelude, PreludeDef (..)) where

import Control.Monad
import Data.Text (Text)
import Interpreter.Value
import Syntax
import VName

type Prelude v m = [PreludeDef v m]

data PreludeDef v m
  = PreludeVal v (Type v) (Val m)
  | PreludeType (TVar v)

prelude :: (Monad m) => Prelude Text m
prelude =
  [ PreludeVal
      "head"
      ( Forall
          [AtomTVar "t"]
          ( DProd
              [DVar "d", SVar "s"]
              ( [ TArr
                    (TVar (AtomTVar "t"))
                    ( Concat
                        [ ShapeDim (Add [Dim 1, DimVar "d"]),
                          ShapeVar "s"
                        ]
                    )
                ]
                  :-> TArr (TVar $ AtomTVar "t") (ShapeVar "s")
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
          [AtomTVar "t"]
          ( DProd
              [DVar "d", SVar "s"]
              ( [ TArr
                    (TVar (AtomTVar "t"))
                    ( Concat
                        [ ShapeDim (Add [Dim 1, DimVar "d"]),
                          ShapeVar "s"
                        ]
                    )
                ]
                  :-> TArr (TVar (AtomTVar "t")) (ShapeVar "s")
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
          [AtomTVar "t"]
          ( DProd
              [DVar "d", SVar "s"]
              ( [TArr (TVar (AtomTVar "t")) (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])]
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
          [AtomTVar "t"]
          ( DProd
              [DVar "m", DVar "n", SVar "s"]
              ( [ TArr (TVar (AtomTVar "t")) (Concat [ShapeDim $ DimVar "m", ShapeVar "s"]),
                  TArr (TVar (AtomTVar "t")) (Concat [ShapeDim $ DimVar "n", ShapeVar "s"])
                ]
                  :-> TArr
                    (TVar (AtomTVar "t"))
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
          [AtomTVar "t"]
          ( DProd
              [DVar "d", SVar "s"]
              ( let arr_t =
                      TArr
                        (TVar (AtomTVar "t"))
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
      ([TArr Int mempty, TArr Int mempty] :-> TArr Int mempty)
      ( ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
          pure $ ValArray mempty [ValBase $ IntVal $ x + y]
      ),
    PreludeVal
      "-"
      ([TArr Int mempty, TArr Int mempty] :-> TArr Int mempty)
      ( ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
          pure $ ValArray mempty [ValBase $ IntVal $ x - y]
      ),
    PreludeVal
      "sum"
      ( DProd
          [SVar "s"]
          ( [ TArr
                Int
                (ShapeVar "s")
            ]
              :-> TArr Int mempty
          )
      )
      ( let valSum :: Val m -> Int
            valSum (ValBase (IntVal x)) = x
            valSum (ValArray _ vs) = sum $ map valSum vs
         in ValIFun $ \[_s] ->
              pure $ ValFun $ \[xs] ->
                pure $ ValArray mempty [ValBase $ IntVal $ valSum xs]
      )
  ]
