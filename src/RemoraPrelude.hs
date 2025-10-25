{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RemoraPrelude
  ( prelude,
    Prelude,
    PreludeVal (..),
  )
where

import Control.Monad
import Data.Text (Text)
import Interpreter.Value
import Syntax
import Util

type Prelude v m = [PreludeVal v m]

data PreludeVal v m = PreludeVal v (Type v) (Val m)

prelude :: (Monad m) => Prelude Text m
prelude =
  [ PreludeVal
      "head"
      ( Forall
          [AtomTVar "t"]
          ( Prod
              [DVar "d", SVar "s"]
              ( [ TArr
                    (TVar (AtomTVar "t"))
                    ( Concat
                        [ ShapeDim (Add [DimN 1, DimVar "d"]),
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
          ( Prod
              [DVar "d", SVar "s"]
              ( [ TArr
                    (TVar (AtomTVar "t"))
                    ( Concat
                        [ ShapeDim (Add [DimN 1, DimVar "d"]),
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
              pure $ ValArray (d - 1 : ds) $ drop 1 vs
      ),
    PreludeVal
      "length"
      ( Forall
          [AtomTVar "t"]
          ( Prod
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
          ( Prod
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
          pure $ ValIFun $ \[Left m, Left n, Right s] ->
            pure $ ValFun $ \[ValArray _ xs, ValArray _ ys] ->
              pure $ ValArray (m + n : s) (xs ++ ys)
      ),
    PreludeVal
      "reverse"
      ( Forall
          [AtomTVar "t"]
          ( Prod
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
          pure $ ValIFun $ \[Left _d, Right _s] ->
            pure $ ValFun $ \[ValArray shape xs] ->
              pure $ ValArray shape (reverse xs)
      ),
    PreludeVal
      "reduce"
      ( Forall
          [AtomTVar "t"]
          ( Prod
              [DVar "d", SVar "s"]
              ( let elem_type = TArr (TVar (AtomTVar "t")) (ShapeVar "s")
                    op_type = TArr ([elem_type, elem_type] :-> elem_type) mempty
                    arg_type =
                      TArr
                        (TVar (AtomTVar "t"))
                        (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                    res_type = TArr (TVar (AtomTVar "t")) (ShapeVar "s")
                 in [op_type, arg_type] :-> res_type
              )
          )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[Left _d, Right s] ->
            pure $ ValFun $ \[ValArray _ [ValFun op], ValArray _ (v : vs)] ->
              foldM (\l r -> op [l, r]) v vs
      ),
    PreludeVal
      "+"
      ([TArr Int mempty, TArr Int mempty] :-> TArr Int mempty)
      ( ValFun $ \[x, y] ->
          arrayValView x $ \(_, [ValBase (IntVal x)]) ->
            arrayValView y $ \(_, [ValBase (IntVal y)]) ->
              pure $ ValArray mempty [ValBase $ IntVal $ x + y]
      ),
    PreludeVal
      "-"
      ([TArr Int mempty, TArr Int mempty] :-> TArr Int mempty)
      ( ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
          pure $ ValArray mempty [ValBase $ IntVal $ x - y]
      ),
    PreludeVal
      "*"
      ([TArr Int mempty, TArr Int mempty] :-> TArr Int mempty)
      ( ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
          pure $ ValArray mempty [ValBase $ IntVal $ x * y]
      ),
    PreludeVal
      "f.+"
      ([TArr Float mempty, TArr Float mempty] :-> TArr Float mempty)
      ( ValFun $ \[ValArray _ [ValBase (FloatVal x)], ValArray _ [ValBase (FloatVal y)]] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x + y]
      ),
    PreludeVal
      "f.-"
      ([TArr Float mempty, TArr Float mempty] :-> TArr Float mempty)
      ( ValFun $ \[ValArray _ [ValBase (FloatVal x)], ValArray _ [ValBase (FloatVal y)]] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x - y]
      ),
    PreludeVal
      "f.*"
      ([TArr Float mempty, TArr Float mempty] :-> TArr Float mempty)
      ( ValFun $ \[ValArray _ [ValBase (FloatVal x)], ValArray _ [ValBase (FloatVal y)]] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x * y]
      ),
    PreludeVal
      "f./"
      ([TArr Float mempty, TArr Float mempty] :-> TArr Float mempty)
      ( ValFun $ \[ValArray _ [ValBase (FloatVal x)], ValArray _ [ValBase (FloatVal y)]] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x / y]
      ),
    PreludeVal
      "sqrt"
      ([TArr Float mempty] :-> TArr Float mempty)
      ( ValFun $ \[ValArray _ [ValBase (FloatVal x)]] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ sqrt x]
      ),
    PreludeVal
      "sum"
      ( Prod
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
            valSum _ = error "valSum"
         in ValIFun $ \[_s] ->
              pure $ ValFun $ \[xs] ->
                pure $ ValArray mempty [ValBase $ IntVal $ valSum xs]
      )
  ]
