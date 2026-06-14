{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module RemoraPrelude
  ( prelude,
    Prelude,
    PreludeVal (..),
  )
where

import Control.Monad
import Data.Bits
import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Debug.Trace (trace, traceM)
import Interpreter qualified as I
import Interpreter.Value
import Parser
import Syntax
import System.IO.Unsafe (unsafePerformIO)
import TypeCheck qualified as TC
import Unsafe.Coerce (unsafeCoerce)
import Util

type Prelude v m = [PreludeVal v m]

data PreludeVal v m = PreludeVal v (ArrayType v) (Val m)

intOps :: (Monad m) => [PreludeVal Text m]
intOps =
  map (uncurry intUnOp) intUnOps
    ++ map (uncurry intBinOp) intBinOps
    ++ map (uncurry intCmp) intCmps
    ++ [ PreludeVal
           "i->f"
           (mkScalarArrayType $ [A Int mempty] :-> A Float mempty)
           ( ValFun $ \args -> baseValViews args $ \[IntVal i] ->
               pure $ ValArray mempty [ValBase $ FloatVal $ fromIntegral i]
           ),
         PreludeVal
           "i->bool"
           (mkScalarArrayType $ [A Int mempty] :-> A Bool mempty)
           ( ValFun $ \args -> baseValViews args $ \[IntVal i] ->
               pure $ ValArray mempty [ValBase $ BoolVal $ i /= 0]
           )
       ]
  where
    intBinOps :: [(Text, Int -> Int -> Int)]
    intBinOps =
      [ ("+", (+)),
        ("-", (-)),
        ("*", (*)),
        ("div", div),
        ("mod", mod),
        ("max", max),
        ("min", min),
        ("bit-and", (.&.)),
        ("bit-or", (.|.)),
        ("bit-xor", xor),
        ("shl", shiftL),
        ("shr", shiftR)
      ]

    intUnOps :: [(Text, Int -> Int)]
    intUnOps =
      [ ("bit-not", complement),
        ("popc", popCount)
      ]

    intCmps :: [(Text, Int -> Int -> Bool)]
    intCmps =
      [ ("==", (==)),
        ("!=", (/=)),
        ("<", (<)),
        (">", (>)),
        ("<=", (<=)),
        (">=", (>=))
      ]

    intBinOp :: (Monad m) => Text -> (Int -> Int -> Int) -> PreludeVal Text m
    intBinOp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Int mempty, A Int mempty] :-> A Int mempty)
        ( ValFun $ \args -> baseValViews args $ \[IntVal x, IntVal y] ->
            pure $ ValArray mempty [ValBase $ IntVal $ f x y]
        )

    intUnOp :: (Monad m) => Text -> (Int -> Int) -> PreludeVal Text m
    intUnOp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Int mempty] :-> A Int mempty)
        ( ValFun $ \args -> baseValViews args $ \[IntVal x] ->
            pure $ ValArray mempty [ValBase $ IntVal $ f x]
        )

    intCmp :: (Monad m) => Text -> (Int -> Int -> Bool) -> PreludeVal Text m
    intCmp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Int mempty, A Int mempty] :-> A Bool mempty)
        ( ValFun $ \args -> baseValViews args $ \[IntVal x, IntVal y] ->
            pure $ ValArray mempty [ValBase $ BoolVal $ f x y]
        )

floatOps :: (Monad m) => [PreludeVal Text m]
floatOps =
  map (uncurry floatUnOp) floatUnOps
    ++ map (uncurry floatBinOp) floatBinOps
    ++ map (uncurry floatCmp) floatCmps
    ++ [ PreludeVal
           "truncate"
           (mkScalarArrayType $ [A Float mempty] :-> A Int mempty)
           ( ValFun $ \args -> baseValViews args $ \[FloatVal x] ->
               pure $ ValArray mempty [ValBase $ IntVal $ truncate x]
           ),
         PreludeVal
           "round"
           (mkScalarArrayType $ [A Float mempty] :-> A Int mempty)
           ( ValFun $ \args -> baseValViews args $ \[FloatVal x] ->
               pure $ ValArray mempty [ValBase $ IntVal $ round x]
           ),
         PreludeVal
           "ceiling"
           (mkScalarArrayType $ [A Float mempty] :-> A Int mempty)
           ( ValFun $ \args -> baseValViews args $ \[FloatVal x] ->
               pure $ ValArray mempty [ValBase $ IntVal $ ceiling x]
           ),
         PreludeVal
           "floor"
           (mkScalarArrayType $ [A Float mempty] :-> A Int mempty)
           ( ValFun $ \args -> baseValViews args $ \[FloatVal x] ->
               pure $ ValArray mempty [ValBase $ IntVal $ floor x]
           )
       ]
  where
    floatUnOps :: [(Text, Float -> Float)]
    floatUnOps =
      [("sqrt", sqrt)]

    floatBinOps :: [(Text, Float -> Float -> Float)]
    floatBinOps =
      [ ("f.+", (+)),
        ("f.^", (**)),
        ("f.-", (-)),
        ("f.*", (*)),
        ("f./", (/)),
        ("f.max", max),
        ("f.min", min)
      ]

    floatCmps :: [(Text, Float -> Float -> Bool)]
    floatCmps =
      [ ("f.==", (==)),
        ("f.!=", (/=)),
        ("f.<", (<)),
        ("f.>", (>)),
        ("f.<=", (<=)),
        ("f.>=", (>=))
      ]

    floatBinOp :: (Monad m) => Text -> (Float -> Float -> Float) -> PreludeVal Text m
    floatBinOp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Float mempty, A Float mempty] :-> A Float mempty)
        ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
            pure $ ValArray mempty [ValBase $ FloatVal $ f x y]
        )

    floatCmp :: (Monad m) => Text -> (Float -> Float -> Bool) -> PreludeVal Text m
    floatCmp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Float mempty, A Float mempty] :-> A Bool mempty)
        ( ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
            pure $ ValArray mempty [ValBase $ BoolVal $ f x y]
        )

    floatUnOp :: (Monad m) => Text -> (Float -> Float) -> PreludeVal Text m
    floatUnOp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Float mempty] :-> A Float mempty)
        ( ValFun $ \args -> baseValViews args $ \[FloatVal x] ->
            pure $ ValArray mempty [ValBase $ FloatVal $ f x]
        )

boolOps :: (Monad m) => [PreludeVal Text m]
boolOps =
  map (uncurry boolUnOp) boolUnOps
    ++ map (uncurry boolBinOp) boolBinOps
    ++ [ PreludeVal
           "bool->i"
           (mkScalarArrayType $ [A Bool mempty] :-> A Int mempty)
           ( ValFun $ \args -> baseValViews args $ \[BoolVal b] ->
               pure $ ValArray mempty [ValBase $ IntVal $ fromEnum b]
           ),
         PreludeVal
           "bool->f"
           (mkScalarArrayType $ [A Bool mempty] :-> A Float mempty)
           ( ValFun $ \args -> baseValViews args $ \[BoolVal b] ->
               pure $ ValArray mempty [ValBase $ FloatVal $ fromIntegral $ fromEnum b]
           )
       ]
  where
    boolUnOps :: [(Text, Bool -> Bool)]
    boolUnOps =
      [("not", not)]

    boolBinOps :: [(Text, Bool -> Bool -> Bool)]
    boolBinOps =
      [ ("and", (&&)),
        ("or", (||)),
        ("bool.==", (==)),
        ("bool.!=", (/=))
      ]

    boolBinOp :: (Monad m) => Text -> (Bool -> Bool -> Bool) -> PreludeVal Text m
    boolBinOp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Bool mempty, A Bool mempty] :-> A Bool mempty)
        ( ValFun $ \args -> baseValViews args $ \[BoolVal x, BoolVal y] ->
            pure $ ValArray mempty [ValBase $ BoolVal $ f x y]
        )

    boolUnOp :: (Monad m) => Text -> (Bool -> Bool) -> PreludeVal Text m
    boolUnOp name f =
      PreludeVal
        name
        (mkScalarArrayType $ [A Bool mempty] :-> A Bool mempty)
        ( ValFun $ \args -> baseValViews args $ \[BoolVal x] ->
            pure $ ValArray mempty [ValBase $ BoolVal $ f x]
        )

prelude :: (Monad m) => Prelude Text m
prelude =
  intOps
    ++ floatOps
    ++ boolOps
    ++ [ PreludeVal
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
               pure $ ValIFun $ \[_d, _s] -> do
                 pure $ ValFun $ \[ValArray (_ : ds) vs] -> do
                   pure $ ValArray ds $ take (product ds) vs
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
                                 :-> A (AtomTypeVar "t") (Concat [ShapeDim $ DimVar "d", (ShapeVar "s")])
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
                   pure $ ValArray (d - 1 : ds) $ drop (product ds) vs
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
                   pure $ ValBase $ IntVal d
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
                                       [ ShapeDim $ Add [DimVar "m", DimVar "n"],
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
                 pure $ ValFun $ \[ValArray _ [ValFun op], ValArray _ vs] ->
                   let (v' : vs') = map (ValArray s) (split (product s) vs)
                    in foldM (\l r -> op [l, r]) v' vs'
           ),
         PreludeVal
           "fold"
           ( mkScalarArrayType $
               Forall
                 [AtomTypeParam "t", AtomTypeParam "t2"]
                 ( A
                     ( Pi
                         [DimParam "d", ShapeParam "s", ShapeParam "s2"]
                         ( let elem_type = A (AtomTypeVar "t") (ShapeVar "s")
                               acc_type = A (AtomTypeVar "t2") (ShapeVar "s2")
                               op_type = A ([acc_type, elem_type] :-> acc_type) mempty
                               arg_type =
                                 A
                                   (AtomTypeVar "t")
                                   (ShapeDim (DimN 1 <> DimVar "d") <> ShapeVar "s")
                            in -- res_type = A (AtomTypeVar "t") (ShapeVar "s")
                               A ([op_type, acc_type, arg_type] :-> acc_type) mempty
                         )
                     )
                     mempty
                 )
           )
           ( ValTFun $ \[_t, _t2] ->
               pure $ ValIFun $ \[Left _d, Right s, Right _s2] ->
                 pure $ ValFun $ \[ValArray _ [ValFun op], zero, ValArray _ vs] ->
                   let vs' = map (ValArray s) (split (product s) vs)
                    in foldM (\l r -> op [l, r]) zero vs'
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
           "iota/static"
           ( mkScalarArrayType $
               Pi [ShapeParam "s"] $
                 A Int (ShapeVar "s")
           )
           ( ValIFun $ \[Right s] ->
               pure $ ValArray s (map (ValBase . IntVal) [0 .. product s - 1])
           ),
         PreludeVal
           "transpose2d"
           ( mkScalarArrayType $
               Forall
                 [AtomTypeParam "t"]
                 ( mkScalarArrayType $
                     ( Pi
                         [DimParam "m", DimParam "n"]
                         ( let arg_t = A (AtomTypeVar "t") (Concat $ map (ShapeDim . DimVar) ["m", "n"])
                               ret_t = A (AtomTypeVar "t") (Concat $ map (ShapeDim . DimVar) ["n", "m"])
                            in mkScalarArrayType $ [arg_t] :-> ret_t
                         )
                     )
                 )
           )
           ( ValTFun $ \_ ->
               pure $ ValIFun $ \[Left _m, Left _n] ->
                 pure $ ValFun $ \[ValArray [m, n] elts] ->
                   pure $
                     ValArray
                       [n, m]
                       (concat $ L.transpose $ split n elts)
           ),
         PreludeVal
           "undefined"
           ( mkScalarArrayType $
               Forall
                 [AtomTypeParam "t"]
                 (mkScalarArrayType (Pi [ShapeParam "s"] $ A (AtomTypeVar "t") (ShapeVar "s")))
           )
           undefined,
         PreludeVal
           "index2d"
           ( mkScalarArrayType $
               Forall [AtomTypeParam "t"] $
                 mkScalarArrayType
                   ( Pi
                       [DimParam "m", DimParam "n"]
                       ( let arr_t = A (AtomTypeVar "t") $ Concat $ map (ShapeDim . DimVar) ["m", "n"]
                             idx_t = A Int $ ShapeDim $ DimN 2
                             ret_t = A (AtomTypeVar "t") $ Concat []
                          in mkScalarArrayType $ [arr_t, idx_t] :-> ret_t
                       )
                   )
           )
           ( ValTFun $ \_ ->
               pure $ ValIFun $ \[_, _] ->
                 pure $ ValFun $ \[ValArray [_, n] elts, ValArray [2] [ValBase (IntVal i), ValBase (IntVal j)]] ->
                   pure $ elts L.!! (n * i + j)
           ),
         PreludeVal
           "flatten/f/3-9-32"
           ( mkScalarArrayType
               ( [A Float (Concat [ShapeDim $ DimN 3, ShapeDim $ DimN 9, ShapeDim $ DimN 32])]
                   :-> A Float (Concat [ShapeDim $ DimN 27, ShapeDim $ DimN 32])
               )
           )
           (ValFun $ \[ValArray _ vs] -> pure $ ValArray [27, 32] vs),
         PreludeVal
           "flatten/f/3-3-32"
           ( mkScalarArrayType
               ( [A Float (Concat [ShapeDim $ DimN 3, ShapeDim $ DimN 3, ShapeDim $ DimN 32])]
                   :-> A Float (Concat [ShapeDim $ DimN 9, ShapeDim $ DimN 32])
               )
           )
           (ValFun $ \[ValArray _ vs] -> pure $ ValArray [9, 32] vs),
         PreludeVal
           "flatten/f/3-9-_"
           ( mkScalarArrayType
               ( [A Float (Concat [ShapeDim $ DimN 3, ShapeDim $ DimN 9])]
                   :-> A Float (Concat [ShapeDim $ DimN 27])
               )
           )
           (ValFun $ \[ValArray _ vs] -> pure $ ValArray [27] vs),
         PreludeVal
           "flatten/f/3-3-_"
           ( mkScalarArrayType
               ( [A Float (Concat [ShapeDim $ DimN 3, ShapeDim $ DimN 3])]
                   :-> A Float (Concat [ShapeDim $ DimN 9])
               )
           )
           (ValFun $ \[ValArray _ vs] -> pure $ ValArray [9] vs),
         PreludeVal
           "flatten/f/608-608-27"
           ( mkScalarArrayType
               ( [A Float (Concat [ShapeDim $ DimN 608, ShapeDim $ DimN 608, ShapeDim $ DimN 27])]
                   :-> A Float (Concat [ShapeDim $ DimN (608 * 608), ShapeDim $ DimN 27])
               )
           )
           (ValFun $ \[ValArray _ vs] -> pure $ ValArray [608 * 608, 27] vs),
         PreludeVal
           "iota/3"
           (A Int $ ShapeDim $ DimN 3)
           (ValArray [3] $ map (ValBase . IntVal) [0, 1, 2]),
         PreludeVal
           "iota/608"
           (A Int $ ShapeDim $ DimN 608)
           (ValArray [608] $ map (ValBase . IntVal) [0 .. 607]),
         PreludeVal
           "iota/610"
           (A Int $ ShapeDim $ DimN 610)
           (ValArray [610] $ map (ValBase . IntVal) [0 .. 609]),
         PreludeVal
           "append/f/608-1-_"
           ( mkScalarArrayType $
               [A Float $ ShapeDim $ DimN 608, A Float $ ShapeDim $ DimN 1]
                 :-> (A Float $ ShapeDim $ DimN 609)
           )
           (ValFun $ \[ValArray _ vsl, ValArray _ vsr] -> pure $ ValArray [609] (vsl ++ vsr)),
         PreludeVal
           "append/f/1-609-_"
           ( mkScalarArrayType $
               [A Float $ ShapeDim $ DimN 1, A Float $ ShapeDim $ DimN 609]
                 :-> (A Float $ ShapeDim $ DimN 610)
           )
           (ValFun $ \[ValArray _ vsl, ValArray _ vsr] -> pure $ ValArray [610] (vsl ++ vsr)),
         PreludeVal
           "append/f/608-1-610"
           ( mkScalarArrayType $
               [ A Float (Concat [ShapeDim $ DimN 608, ShapeDim $ DimN 610]),
                 A Float (Concat [ShapeDim $ DimN 1, ShapeDim $ DimN 610])
               ]
                 :-> (A Float (Concat [ShapeDim $ DimN 609, ShapeDim $ DimN 610]))
           )
           (ValFun $ \[ValArray _ vsl, ValArray _ vsr] -> pure $ ValArray [609, 610] (vsl ++ vsr)),
         PreludeVal
           "append/f/1-609-610"
           ( mkScalarArrayType $
               [ A Float (Concat [ShapeDim $ DimN 1, ShapeDim $ DimN 610]),
                 A Float (Concat [ShapeDim $ DimN 609, ShapeDim $ DimN 610])
               ]
                 :-> (A Float (Concat [ShapeDim $ DimN 610, ShapeDim $ DimN 610]))
           )
           (ValFun $ \[ValArray _ vsl, ValArray _ vsr] -> pure $ ValArray [610, 610] (vsl ++ vsr)),
         PreludeVal
           "transpose/f/27-32"
           ( mkScalarArrayType $
               [A Float (Concat [ShapeDim $ DimN 27, ShapeDim $ DimN 32])]
                 :-> (A Float (Concat [ShapeDim $ DimN 32, ShapeDim $ DimN 27]))
           )
           (ValFun $ \[ValArray _ vs] -> pure $ ValArray [32, 27] (concat $ L.transpose $ split 32 vs)),
         PreludeVal
           "reduce/f/26"
           ( mkScalarArrayType $
               let elem_type = A Float mempty
                   op_type = A ([elem_type, elem_type] :-> elem_type) mempty
                   arg_type =
                     A
                       Float
                       (ShapeDim (DimN 27))
                   res_type = A Float mempty
                in [op_type, arg_type] :-> res_type
           )
           ( ValFun $ \[ValArray _ [ValFun op], ValArray _ (v : vs)] ->
               foldM (\l r -> op [l, r]) v vs
           ),
         PreludeVal
           "index2d/f/610"
           ( mkScalarArrayType $
               [ A Float (Concat [ShapeDim $ DimN 610, ShapeDim $ DimN 610]),
                 A Int $ ShapeDim $ DimN 2
               ]
                 :-> (A Float mempty)
           )
           ( ValFun $ \[ValArray _ vs, ValArray _ [ValBase (IntVal i), ValBase (IntVal j)]] ->
               pure (vs !! (i * 610 + j))
           ),
         PreludeVal
           "undefined-input"
           (A Float $ Concat [ShapeDim $ DimN 3, ShapeDim $ DimN 608, ShapeDim $ DimN 608])
           undefined,
         PreludeVal
           "undefined-weights"
           (A Float $ Concat [ShapeDim $ DimN 32, ShapeDim $ DimN 3, ShapeDim $ DimN 3, ShapeDim $ DimN 3])
           undefined,
         PreludeVal
           "trace"
           ( mkScalarArrayType $
               Forall [AtomTypeParam "t", AtomTypeParam "r"] $
                 mkScalarArrayType
                   ( Pi [ShapeParam "s", ShapeParam "q"] $
                       mkScalarArrayType $
                         [A (AtomTypeVar "t") (ShapeVar "s"), A (AtomTypeVar "r") (ShapeVar "q")]
                           :-> A (AtomTypeVar "r") (ShapeVar "q")
                   )
           )
           ( ValTFun $ \_ ->
               pure $ ValIFun $ \_ ->
                 pure $ ValFun $ \[log, v] ->
                   pure $ trace (prettyString log) v
           ),
         PreludeVal
           "trace-file"
           ( mkScalarArrayType $
               Forall [AtomTypeParam "t", AtomTypeParam "r"] $
                 mkScalarArrayType
                   ( Pi [DimParam "d", ShapeParam "s", ShapeParam "q"] $
                       mkScalarArrayType $
                         [ A Int (ShapeDim (DimVar "d")),
                           A (AtomTypeVar "t") (ShapeVar "s"),
                           A (AtomTypeVar "r") (ShapeVar "q")
                         ]
                           :-> A (AtomTypeVar "r") (ShapeVar "q")
                   )
           )
           ( ValTFun $ \_ ->
               pure $ ValIFun $ \_ ->
                 pure $ ValFun $ \[filename, log, v] ->
                   pure $ unsafePerformIO $ do
                     appendFile (valToString filename) (prettyString log ++ "\n")
                     pure v
           ),
         PreludeVal
           "read-file"
           ( mkScalarArrayType $
               Forall [AtomTypeParam "t"] $
                 mkScalarArrayType
                   ( Pi [DimParam "d", ShapeParam "s"] $
                       mkScalarArrayType $
                         [A Int (ShapeDim (DimVar "d"))]
                           :-> A (AtomTypeVar "t") (ShapeVar "s")
                   )
           )
           ( ValTFun $ \_ ->
               pure $ ValIFun $ \_ ->
                 pure $ ValFun $ \[filename] ->
                   pure $ unsafePerformIO $ do
                     let file = valToString filename
                     input <- T.readFile file
                     let mv = do
                           expr <- parse "<read-file>" input
                           (p, expr') <- TC.check expr
                           I.interpret p expr'
                     case mv of
                       Left err -> error $ "read-file:" <> T.unpack err
                       Right v -> pure $ unsafeCoerce v
           )
       ]
