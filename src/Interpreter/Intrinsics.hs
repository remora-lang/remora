module Interpreter.Intrinsics
  ( Intrinsics,
    IntrinsicVal (..),
    intrinsics,
  )
where

import Control.Monad
import Data.Bits hiding (And)
import Data.List qualified as L
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace (trace)
import Interpreter.Value
import Intrinsics qualified
import Primitive
import Syntax hiding (Add, Mul, Sub)
import System.IO.Unsafe (unsafePerformIO)
import Util
import VName

type Intrinsics m = [IntrinsicVal m]

data IntrinsicVal m = IntrinsicVal VName (ArrayType VName) (Val m)

fun2 ::
  (Applicative m) =>
  ((a -> m (Val m)) -> Val m) -> (a -> a -> m (Val m)) -> Val m
fun2 wrap f = wrap $ \x -> pure $ wrap (f x)

fun3 ::
  (Applicative m) =>
  ((a -> m (Val m)) -> Val m) -> (a -> a -> a -> m (Val m)) -> Val m
fun3 wrap f = wrap $ \x -> pure $ fun2 wrap (f x)

applyFun :: (Monad m) => Val m -> Val m -> Val m -> m (Val m)
applyFun (ValFun f) l r = do
  g <- f l
  case asScalar g of
    ValFun g' -> g' r
    _ -> error "applyFun: partial application did not yield a function"
applyFun _ _ _ = error "applyFun: not a function"

scalarVal :: Base -> Val m
scalarVal b = ValArray mempty [ValBase b]

asDim :: Either Int [Int] -> Int
asDim = either id (error "asDim: expected a dimension, got a shape")

asShape :: Either Int [Int] -> [Int]
asShape = either (error "asShape: expected a shape, got a dimension") id

intrinsics :: forall m. (Monad m) => Intrinsics m
intrinsics =
  flip map (M.toList Intrinsics.intrinsics) $ \(vname, t) ->
    IntrinsicVal vname t (val $ varName vname)
  where
    tFun1 = ValTFun
    tFun2 = fun2 ValTFun
    iFun1 = ValIFun
    iFun2 = fun2 ValIFun
    iFun3 = fun3 ValIFun
    vFun1 = ValFun
    vFun2 = fun2 ValFun
    vFun3 = fun3 ValFun

    intBin f = vFun2 $ \x y ->
      pure $ scalarVal $ IntVal $ f (asInt x) (asInt y)
    intCmp f = vFun2 $ \x y ->
      pure $ scalarVal $ BoolVal $ f (asInt x) (asInt y)
    intUn f = vFun1 $ \x ->
      pure $ scalarVal $ IntVal $ f (asInt x)
    floatBin f = vFun2 $ \x y ->
      pure $ scalarVal $ FloatVal $ f (asFloat x) (asFloat y)
    floatCmp f = vFun2 $ \x y ->
      pure $ scalarVal $ BoolVal $ f (asFloat x) (asFloat y)
    floatUn f = vFun1 $ \x ->
      pure $ scalarVal $ FloatVal $ f (asFloat x)
    boolBin f = vFun2 $ \x y ->
      pure $ scalarVal $ BoolVal $ f (asBool x) (asBool y)
    boolUn f = vFun1 $ \x ->
      pure $ scalarVal $ BoolVal $ f (asBool x)
    conv f = vFun1 $ \x ->
      pure $ scalarVal (f x)

    binOpVal :: BinOp -> Val m
    binOpVal Add = intBin (+)
    binOpVal Sub = intBin (-)
    binOpVal Mul = intBin (*)
    binOpVal Div = intBin div
    binOpVal Pow = intBin (^)
    binOpVal Mod = intBin mod
    binOpVal Max = intBin max
    binOpVal Min = intBin min
    binOpVal BitAnd = intBin (.&.)
    binOpVal BitOr = intBin (.|.)
    binOpVal BitXor = intBin xor
    binOpVal Shl = intBin shiftL
    binOpVal Shr = intBin shiftR
    binOpVal Eq = intCmp (==)
    binOpVal Neq = intCmp (/=)
    binOpVal Lt = intCmp (<)
    binOpVal Gt = intCmp (>)
    binOpVal Le = intCmp (<=)
    binOpVal Ge = intCmp (>=)
    binOpVal FAdd = floatBin (+)
    binOpVal FSub = floatBin (-)
    binOpVal FMul = floatBin (*)
    binOpVal FDiv = floatBin (/)
    binOpVal FPow = floatBin (**)
    binOpVal FMax = floatBin max
    binOpVal FMin = floatBin min
    binOpVal FEq = floatCmp (==)
    binOpVal FNeq = floatCmp (/=)
    binOpVal FLt = floatCmp (<)
    binOpVal FGt = floatCmp (>)
    binOpVal FLe = floatCmp (<=)
    binOpVal FGe = floatCmp (>=)
    binOpVal And = boolBin (&&)
    binOpVal Or = boolBin (||)
    binOpVal BEq = boolBin (==)
    binOpVal BNeq = boolBin (/=)

    unOpVal :: UnOp -> Val m
    unOpVal BitNot = intUn complement
    unOpVal Popc = intUn popCount
    unOpVal Sqrt = floatUn sqrt
    unOpVal FSqrt = floatUn sqrt
    unOpVal Not = boolUn not
    unOpVal IntToFloat = conv $ FloatVal . fromIntegral . asInt
    unOpVal IntToBool = conv $ \x -> BoolVal (asInt x /= 0)
    unOpVal BoolToInt = conv $ IntVal . fromEnum . asBool
    unOpVal BoolToFloat = conv $ FloatVal . fromIntegral . fromEnum . asBool
    unOpVal Truncate = conv $ IntVal . truncate . asFloat
    unOpVal Round = conv $ IntVal . round . asFloat
    unOpVal Ceiling = conv $ IntVal . ceiling . asFloat
    unOpVal Floor = conv $ IntVal . floor . asFloat

    scalarOps :: Map Text (Val m)
    scalarOps =
      M.fromList $
        [(prettyText op, binOpVal op) | (op, _) <- binOps]
          ++ [(prettyText op, unOpVal op) | (op, _) <- unOps]

    val :: Text -> Val m
    val name = M.findWithDefault (intrinsic name) name scalarOps

    intrinsic :: Text -> Val m
    intrinsic "head" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \_ _ ->
          pure $ vFun1 $ \arr ->
            let (shape, vs) = asArray arr
                ds = drop 1 shape
             in pure $ ValArray ds $ take (product ds) vs
    intrinsic "tail" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \_ _ ->
          pure $ vFun1 $ \arr ->
            case asArray arr of
              (d : ds, vs) -> pure $ ValArray (d - 1 : ds) $ drop (product ds) vs
              _ -> error "tail: empty shape"
    intrinsic "length" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \_ _ ->
          pure $ vFun1 $ \arr ->
            case asArray arr of
              (d : _, _) -> pure $ ValBase $ IntVal d
              _ -> error "length: empty shape"
    intrinsic "append" =
      tFun1 $ \_ ->
        pure $ iFun3 $ \m n s ->
          pure $ vFun2 $ \xs ys ->
            pure $ ValArray (asDim m + asDim n : asShape s) (snd (asArray xs) ++ snd (asArray ys))
    intrinsic "reverse" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \_ _ ->
          pure $ vFun1 $ \arr ->
            let (shape, xs) = asArray arr
             in pure $ ValArray shape (reverse xs)
    intrinsic "reduce" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \_ s ->
          pure $ vFun2 $ \opv arr ->
            let op = asScalar opv
                s' = asShape s
                cells = map (ValArray s') $ split (product s') $ snd $ asArray arr
             in case cells of
                  c : cs -> foldM (applyFun op) c cs
                  [] -> error "reduce: empty array"
    intrinsic "fold" =
      tFun2 $ \_ _ ->
        pure $ iFun3 $ \_ s _ ->
          pure $ vFun3 $ \opv zero arr ->
            let op = asScalar opv
                s' = asShape s
                cells = map (ValArray s') $ split (product s') $ snd $ asArray arr
             in foldM (applyFun op) zero cells
    intrinsic "sum" =
      let valSum (ValBase (IntVal x)) = x
          valSum (ValArray _ vs) = Prelude.sum $ map valSum vs
          valSum _ = error "valSum"
       in iFun1 $ \_ ->
            pure $ vFun1 $ \xs ->
              pure $ ValArray mempty [ValBase $ IntVal $ valSum xs]
    intrinsic "flatten" =
      tFun1 $ \_ ->
        pure $ iFun3 $ \_ _ _ ->
          pure $ vFun1 $ \v -> pure $ valConcat v
    -- intrinsic "reshape" =
    --   tFun1 $ \_ ->
    --     pure $ iFun2 $ \(Right oldShp) (Right newShp) ->
    --       pure $ vFun1 $ \(ValArray _ vs) -> pure $ assert () $ ValArray newShp vs
    intrinsic "reshape" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \s1 s2 ->
          case (s1, s2) of
            (Right oldShp, Right newShp) | product oldShp == product newShp ->
              pure $ vFun1 $ \v -> case v of
                ValArray _ vs -> pure $ ValArray newShp vs
                _ -> error "reshape: not an array"
            (Right _, Right _) -> error "reshape: two shapes contain diff # of elements"
            _ -> error "reshape: one of the indices is not a shape"
    intrinsic "iota" =
      iFun1 $ \_ ->
        pure $ vFun1 $ \shapeval ->
          let shape = map asInt $ snd $ asArray shapeval
           in pure $
                ValBox [Right shape] $
                  ValArray shape (map (ValBase . IntVal) [0 .. product shape - 1])
    intrinsic "iota/static" =
      iFun1 $ \s ->
        let s' = asShape s
         in pure $ ValArray s' (map (ValBase . IntVal) [0 .. product s' - 1])
    intrinsic "transpose2d" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \_ _ ->
          pure $ vFun1 $ \arr ->
            case asArray arr of
              ([m, n], elts) -> pure $ ValArray [n, m] (concat $ L.transpose $ split n elts)
              _ -> error "transpose2d: not a 2d array"
    intrinsic "undefined" = undefined
    intrinsic "index" =
      tFun1 $ \_ ->
        pure $ iFun1 $ \_ ->
          pure $ vFun2 $ \arr idx ->
            case asArray arr of
              ([_], elts) -> pure $ elts L.!! asInt idx
              _ -> error "index: bad arguments"
    intrinsic "index2d" =
      tFun1 $ \_ ->
        pure $ iFun2 $ \_ _ ->
          pure $ vFun2 $ \arr idx ->
            case (asArray arr, asArray idx) of
              (([_, n], elts), (_, [i, j])) -> pure $ elts L.!! (n * asInt i + asInt j)
              _ -> error "index2d: bad arguments"
    intrinsic "trace" =
      tFun2 $ \_ _ ->
        pure $ iFun2 $ \_ _ ->
          pure $ vFun2 $ \msg v ->
            pure $ trace (prettyString msg) v
    intrinsic "trace-file" =
      tFun2 $ \_ _ ->
        pure $ iFun3 $ \_ _ _ ->
          pure $ vFun3 $ \filename msg v ->
            pure $ unsafePerformIO $ do
              appendFile (valToString filename) (prettyString msg ++ "\n")
              pure v
    intrinsic "read-file" = undefined
    intrinsic "reify-dim" =
      iFun1 $ pure . ValBase . IntVal . asDim
    intrinsic "reify-shape" =
      iFun1 $ \s ->
        let ds = asShape s
            rank = length ds
         in pure $
              ValBox [Left rank] $
                ValArray [rank] (ValBase . IntVal <$> ds)
    intrinsic x = error $ "intrinsic: " <> T.unpack x
