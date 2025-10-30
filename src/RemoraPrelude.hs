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
      ( ScalarType $
          Forall
            [AtomTVar "t"]
            ( A
                ( Pi
                    [DVar "d", SVar "s"]
                    ( A
                        ( [ A
                              (ScalarTVar "t")
                              ( Concat
                                  [ ShapeDim (Add [DimN 1, DimVar "d"]),
                                    ShapeVar "s"
                                  ]
                              )
                          ]
                            :-> A (ScalarTVar "t") (ShapeVar "s")
                        )
                        mempty
                    )
                )
                mempty
            )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[_d, _s] ->
            pure $ ValFun $ \[ValArray (d : ds) vs] ->
              pure $ ValArray (d - 1 : ds) $ init vs
      ),
    PreludeVal
      "tail"
      ( ScalarType $
          Forall
            [AtomTVar "t"]
            ( A
                ( Pi
                    [DVar "d", SVar "s"]
                    ( A
                        ( [ A
                              (ScalarTVar "t")
                              ( Concat
                                  [ ShapeDim (Add [DimN 1, DimVar "d"]),
                                    ShapeVar "s"
                                  ]
                              )
                          ]
                            :-> A (ScalarTVar "t") (ShapeVar "s")
                        )
                        mempty
                    )
                )
                mempty
            )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[_d, _s] ->
            pure $ ValFun $ \[ValArray (d : ds) vs] ->
              pure $ ValArray (d - 1 : ds) $ drop 1 vs
      ),
    PreludeVal
      "length"
      ( ScalarType $
          Forall
            [AtomTVar "t"]
            ( A
                ( Pi
                    [DVar "d", SVar "s"]
                    ( A
                        ( [A (ScalarTVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])]
                            :-> A Int mempty
                        )
                        mempty
                    )
                )
                mempty
            )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[_d, _s] ->
            pure $ ValFun $ \[ValArray (d : _) _] ->
              pure $ ValBase $ IntVal $ d
      ),
    PreludeVal
      "append"
      ( ScalarType $
          Forall
            [AtomTVar "t"]
            ( A
                ( Pi
                    [DVar "m", DVar "n", SVar "s"]
                    ( A
                        ( [ A (ScalarTVar "t") (Concat [ShapeDim $ DimVar "m", ShapeVar "s"]),
                            A (ScalarTVar "t") (Concat [ShapeDim $ DimVar "n", ShapeVar "s"])
                          ]
                            :-> A
                              (ScalarTVar "t")
                              ( Concat
                                  [ ShapeDim $ DimVar "m",
                                    ShapeDim $ DimVar "n",
                                    ShapeVar "s"
                                  ]
                              )
                        )
                        mempty
                    )
                )
                mempty
            )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[Left m, Left n, Right s] ->
            pure $ ValFun $ \[ValArray _ xs, ValArray _ ys] ->
              pure $ ValArray (m + n : s) (xs ++ ys)
      ),
    PreludeVal
      "reverse"
      ( ScalarType $
          Forall
            [AtomTVar "t"]
            ( A
                ( Pi
                    [DVar "d", SVar "s"]
                    ( let arr_t =
                            A
                              (ScalarTVar "t")
                              ( Concat
                                  [ShapeDim $ DimVar "d", ShapeVar "s"]
                              )
                       in A ([arr_t] :-> arr_t) mempty
                    )
                )
                mempty
            )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[Left _d, Right _s] ->
            pure $ ValFun $ \[ValArray shape xs] ->
              pure $ ValArray shape (reverse xs)
      ),
    PreludeVal
      "reduce"
      ( ScalarType $
          Forall
            [AtomTVar "t"]
            ( A
                ( Pi
                    [DVar "d", SVar "s"]
                    ( let elem_type = A (ScalarTVar "t") (ShapeVar "s")
                          op_type = A ([elem_type, elem_type] :-> elem_type) mempty
                          arg_type =
                            A
                              (ScalarTVar "t")
                              (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                          res_type = A (ScalarTVar "t") (ShapeVar "s")
                       in A ([op_type, arg_type] :-> res_type) mempty
                    )
                )
                mempty
            )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[Left _d, Right s] ->
            pure $ ValFun $ \[ValArray _ [ValFun op], ValArray _ (v : vs)] ->
              foldM (\l r -> op [l, r]) v vs
      ),
    PreludeVal
      "+"
      (ScalarType $ [A Int mempty, A Int mempty] :-> A Int mempty)
      ( ValFun $ \[x, y] ->
          arrayValView x $ \(_, [ValBase (IntVal x)]) ->
            arrayValView y $ \(_, [ValBase (IntVal y)]) ->
              pure $ ValArray mempty [ValBase $ IntVal $ x + y]
      ),
    PreludeVal
      "-"
      (ScalarType $ [A Int mempty, A Int mempty] :-> A Int mempty)
      ( ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
          pure $ ValArray mempty [ValBase $ IntVal $ x - y]
      ),
    PreludeVal
      "*"
      (ScalarType $ [A Int mempty, A Int mempty] :-> A Int mempty)
      ( ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
          pure $ ValArray mempty [ValBase $ IntVal $ x * y]
      ),
    PreludeVal
      "f.+"
      (ScalarType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x + y]
      ),
    PreludeVal
      "f.-"
      (ScalarType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x - y]
      ),
    PreludeVal
      "f.*"
      (ScalarType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x * y]
      ),
    PreludeVal
      "f./"
      (ScalarType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x / y]
      ),
    PreludeVal
      "sqrt"
      (ScalarType $ [A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ sqrt x]
      ),
    PreludeVal
      "sum"
      ( ScalarType $
          Pi
            [SVar "s"]
            ( A
                ( [A Int (ShapeVar "s")]
                    :-> A Int mempty
                )
                mempty
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
