module Interpreter.Intrinsics
  ( Intrinsics,
    IntrinsicVal (..),
    intrinsics,
  )
where

import Control.Monad
import Data.List qualified as L
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Float (int2Float)
import Interpreter.Value
import Intrinsics qualified
import Syntax
import VName

type Intrinsics m = [IntrinsicVal m]

data IntrinsicVal m = IntrinsicVal VName (ArrayType VName) (Val m)

intrinsics :: forall m. (Monad m) => Intrinsics m
intrinsics =
  flip map (M.toList Intrinsics.intrinsics) $ \(vname, t) ->
    IntrinsicVal vname t (int $ varName vname)
  where
    int :: Text -> Val m
    int "+" =
      ValFun $ \[x, y] ->
        arrayValView x $ \(_, [ValBase (IntVal x)]) ->
          arrayValView y $ \(_, [ValBase (IntVal y)]) ->
            pure $ ValArray mempty [ValBase $ IntVal $ x + y]
    int "-" =
      ValFun $ \[ValArray _ [ValBase (IntVal x)], ValArray _ [ValBase (IntVal y)]] ->
        pure $ ValArray mempty [ValBase $ IntVal $ x - y]
    int "*" =
      ValFun $ \[x, y] ->
        arrayValView x $ \(_, [ValBase (IntVal x)]) ->
          arrayValView y $ \(_, [ValBase (IntVal y)]) ->
            pure $ ValArray mempty [ValBase $ IntVal $ x * y]
    int "f.+" =
      ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
        pure $ ValArray mempty [ValBase $ FloatVal $ x + y]
    int "f.^" =
      ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
        pure $ ValArray mempty [ValBase $ FloatVal $ x ** y]
    int "f.-" =
      ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
        pure $ ValArray mempty [ValBase $ FloatVal $ x - y]
    int "f.*" =
      ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
        pure $ ValArray mempty [ValBase $ FloatVal $ x * y]
    int "f./" =
      ValFun $ \args -> baseValViews args $ \[FloatVal x, FloatVal y] ->
        pure $ ValArray mempty [ValBase $ FloatVal $ x / y]
    int "head" =
      ValTFun $ \[_t] ->
        pure $ ValIFun $ \[_d, _s] -> do
          pure $ ValFun $ \[ValArray (_ : ds) vs] -> do
            pure $ ValArray ds $ take (product ds) vs
    int "tail" =
      ValTFun $ \[_t] ->
        pure $ ValIFun $ \[_d, _s] ->
          pure $ ValFun $ \[ValArray (d : ds) vs] ->
            pure $ ValArray (d - 1 : ds) $ drop 1 vs
    int "length" =
      ValTFun $ \[_t] ->
        pure $ ValIFun $ \[_d, _s] ->
          pure $ ValFun $ \[ValArray (d : _) _] ->
            pure $ ValBase $ IntVal d
    int "append" =
      ValTFun $ \[_t] ->
        pure $ ValIFun $ \[Left m, Left n, Right s] ->
          pure $ ValFun $ \[ValArray _ xs, ValArray _ ys] ->
            pure $ ValArray (m + n : s) (xs ++ ys)
    int "reverse" =
      ValTFun $ \[_t] ->
        pure $ ValIFun $ \[Left _d, Right _s] ->
          pure $ ValFun $ \[ValArray shape xs] ->
            pure $ ValArray shape (reverse xs)
    int "reduce" =
      ValTFun $ \[_t] ->
        pure $ ValIFun $ \[Left _d, Right s] ->
          pure $ ValFun $ \[ValArray _ [ValFun op], ValArray _ (v : vs)] ->
            foldM (\l r -> op [l, r]) v vs
    int "sum" =
      let valSum :: Val m -> Int
          valSum (ValBase (IntVal x)) = x
          valSum (ValArray _ vs) = sum $ map valSum vs
          valSum _ = error "valSum"
       in ValIFun $ \[_s] ->
            pure $ ValFun $ \[xs] ->
              pure $ ValArray mempty [ValBase $ IntVal $ valSum xs]
    int "flatten" =
      ValTFun $ \[_t] ->
        pure $ ValIFun $ \[Left _m, Left _n, Right _s] ->
          pure $ ValFun $ \[v] ->
            pure $ valConcat v
    int "iota" =
      ValIFun $ \[Left _d] ->
        pure $ ValFun $ \[shapeval] ->
          let fromInt (IntVal i) = i
              fromInt _ = error "fromInt"
              shape = arrayValView shapeval $ flip baseValViews (map fromInt) . snd
           in pure $
                ValBox [Right shape] $
                  ValArray
                    shape
                    (map (ValBase . IntVal) [0 .. product shape - 1])
    int "iota/static" =
      ValIFun $ \[Right s] ->
        pure $ ValArray s (map (ValBase . IntVal) [0 .. product s - 1])
    int "transpose2d" =
      ValTFun $ \_ ->
        pure $ ValIFun $ \[Left _m, Left _n] ->
          pure $ ValFun $ \[ValArray [m, n] elts] ->
            pure $
              ValArray
                [n, m]
                (concat $ L.transpose $ split n elts)
    int "undefined" = undefined
    int "index2d" =
      ValTFun $ \_ ->
        pure $ ValIFun $ \[_, _] ->
          pure $
            ValFun $
              \[ ValArray [_, n] elts,
                 ValArray [2] [ValBase (IntVal i), ValBase (IntVal j)]
                 ] ->
                  pure $ elts L.!! (n * i + j)
    int "i->f" =
      ValFun $ \[ValArray [] [ValBase (IntVal i)]] ->
        pure $ ValArray [] [ValBase (FloatVal $ int2Float i)]
    int x = error $ "int: " <> T.unpack x
