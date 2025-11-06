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

type Prelude v m = [PreludeVal v m]

data PreludeVal v m = PreludeVal v (ArrayType v) (Val m)

prelude :: (Monad m) => Prelude Text m
prelude =
  [ PreludeVal
      "head"
      ( mkScalarArrayType $
          Forall
            [AtomTypeParam "t"]
            ( A
                ( Pi
                    [DimParam "d", ShapeParam "s"]
                    ( A
                        ( [ A
                              (AtomTypeVar "t")
                              ( Concat
                                  [ ShapeDim (Add [DimN 1, DimVar "d"]),
                                    ShapeVar "s"
                                  ]
                              )
                          ]
                            :-> A (AtomTypeVar "t") (ShapeVar "s")
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
      ( mkScalarArrayType $
          Forall
            [AtomTypeParam "t"]
            ( A
                ( Pi
                    [DimParam "d", ShapeParam "s"]
                    ( A
                        ( [ A
                              (AtomTypeVar "t")
                              ( Concat
                                  [ ShapeDim (Add [DimN 1, DimVar "d"]),
                                    ShapeVar "s"
                                  ]
                              )
                          ]
                            :-> A (AtomTypeVar "t") (ShapeVar "s")
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
      ( mkScalarArrayType $
          Forall
            [AtomTypeParam "t"]
            ( A
                ( Pi
                    [DimParam "d", ShapeParam "s"]
                    ( A
                        ( [A (AtomTypeVar "t") (Concat [ShapeDim (DimVar "d"), ShapeVar "s"])]
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
      ( mkScalarArrayType $
          Forall
            [AtomTypeParam "t"]
            ( A
                ( Pi
                    [DimParam "m", DimParam "n", ShapeParam "s"]
                    ( A
                        ( [ A (AtomTypeVar "t") (Concat [ShapeDim $ DimVar "m", ShapeVar "s"]),
                            A (AtomTypeVar "t") (Concat [ShapeDim $ DimVar "n", ShapeVar "s"])
                          ]
                            :-> A
                              (AtomTypeVar "t")
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
      ( mkScalarArrayType $
          Forall
            [AtomTypeParam "t"]
            ( A
                ( Pi
                    [DimParam "d", ShapeParam "s"]
                    ( let arr_t =
                            A
                              (AtomTypeVar "t")
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
      ( mkScalarArrayType $
          Forall
            [AtomTypeParam "t"]
            ( A
                ( Pi
                    [DimParam "d", ShapeParam "s"]
                    ( let elem_type = A (AtomTypeVar "t") (ShapeVar "s")
                          op_type = A ([elem_type, elem_type] :-> elem_type) mempty
                          arg_type =
                            A
                              (AtomTypeVar "t")
                              (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                          res_type = A (AtomTypeVar "t") (ShapeVar "s")
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
      (mkScalarArrayType $ [A Int mempty, A Int mempty] :-> A Int mempty)
      ( ValFun $ \[x, y] ->
          arrayValView x $ \(_, [ValBase (IntVal x)]) ->
            arrayValView y $ \(_, [ValBase (IntVal y)]) ->
              pure $ ValArray mempty [ValBase $ IntVal $ x + y]
      ),
    PreludeVal
      "-"
      (mkScalarArrayType $ [A Int mempty, A Int mempty] :-> A Int mempty)
      ( ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
          pure $ ValArray mempty [ValBase $ IntVal $ x - y]
      ),
    PreludeVal
      "*"
      (mkScalarArrayType $ [A Int mempty, A Int mempty] :-> A Int mempty)
      ( ValFun $ \[x, y] ->
          arrayValView x $ \(_, [ValBase (IntVal x)]) ->
            arrayValView y $ \(_, [ValBase (IntVal y)]) ->
              pure $ ValArray mempty [ValBase $ IntVal $ x * y]
      ),
    PreludeVal
      "f.+"
      (mkScalarArrayType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x + y]
      ),
    PreludeVal
      "f.^"
      (mkScalarArrayType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x ** y]
      ),
    PreludeVal
      "f.-"
      (mkScalarArrayType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x - y]
      ),
    PreludeVal
      "f.*"
      (mkScalarArrayType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x * y]
      ),
    PreludeVal
      "f./"
      (mkScalarArrayType $ [A Float mempty, A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ x / y]
      ),
    PreludeVal
      "sqrt"
      (mkScalarArrayType $ [A Float mempty] :-> A Float mempty)
      ( ValFun $ \args -> baseValViews args $ \[FloatVal x] ->
          pure $ ValArray mempty [ValBase $ FloatVal $ sqrt x]
      ),
    PreludeVal
      "sum"
      ( mkScalarArrayType $
          Pi
            [ShapeParam "s"]
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
      ),
    PreludeVal
      "flatten"
      ( mkScalarArrayType $
          Forall
            [AtomTypeParam "t"]
            ( A
                ( Pi
                    [DimParam "m", DimParam "n", ShapeParam "s"]
                    ( let arg_t =
                            A
                              (AtomTypeVar "t")
                              ( Concat
                                  [ShapeDim $ DimVar "m", ShapeDim $ DimVar "n", ShapeVar "s"]
                              )
                          ret_t =
                            A
                              (AtomTypeVar "t")
                              ( Concat
                                  [ShapeDim $ (Mul [DimVar "m", DimVar "n"]), ShapeVar "s"]
                              )
                       in A ([arg_t] :-> ret_t) mempty
                    )
                )
                mempty
            )
      )
      ( ValTFun $ \[_t] ->
          pure $ ValIFun $ \[Left _m, Left _n, Right _s] ->
            pure $ ValFun $ \[v] ->
              pure $ valConcat v
      ),
    PreludeVal
      "iota"
      ( mkScalarArrayType $
          Pi
            [DimParam "d"]
            ( let arg_t = A Int (ShapeDim $ DimVar "d")
                  ret_t =
                    mkScalarArrayType $
                      Sigma [ShapeParam "s"] $
                        A Int (ShapeVar "s")
               in mkScalarArrayType $ [arg_t] :-> ret_t
            )
      )
      ( ValIFun $ \[Left _d] ->
          pure $ ValFun $ \[shapeval] ->
            let fromInt (IntVal i) = i
                fromInt _ = error "fromInt"
                shape = arrayValView shapeval $ flip baseValViews (map fromInt) . snd
             in pure $
                  ValBox [Right shape] $
                    ValArray
                      shape
                      (map (ValBase . IntVal) [0 .. product shape - 1])
      ),
    PreludeVal
      "f.reduce3"
      ( mkScalarArrayType $
          let elem_type = A Float mempty
              op_type = A ([elem_type, elem_type] :-> elem_type) mempty
              arg_type =
                A
                  Float
                  (ShapeDim (DimN 3))
              res_type = A Float mempty
           in [op_type, arg_type] :-> res_type
      )
      ( ValFun $ \[ValArray _ [ValFun op], ValArray _ (v : vs)] ->
          foldM (\l r -> op [l, r]) v vs
      )
  ]
