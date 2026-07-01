module Interpreter.Value
  ( Val (..),
    arrayifyVal,
    asBase,
    asInt,
    asFloat,
    asBool,
    asArray,
    asScalar,
    split,
    rep,
    (\\),
    transpose,
    valConcat,
    collapse,
    valShapeOf,
    valToString,
  )
where

import Data.List qualified as L
import Prettyprinter
import Syntax hiding (Atom, AtomType, Exp, ISpace, ISpaceParam, Shape, Type, TypeParam)
import Syntax qualified
import Util
import VName

type Type = Syntax.Type VName

-- | Values. Paramized by a monad @m@ over which function bodies are
-- evaluated.
data Val m
  = -- | Variables.
    ValVar VName
  | -- | Base values.
    ValBase Base
  | -- | Array value.
    ValArray [Int] [Val m]
  | -- | Box.
    ValBox [Either Int [Int]] (Val m)
  | -- | Function.
    ValFun (Val m -> m (Val m))
  | -- | Type function.
    ValTFun (Type -> m (Val m))
  | -- | Index function.
    ValIFun (Either Int [Int] -> m (Val m))

instance Show (Val m) where
  show (ValVar v) = "ValVar " <> show v
  show (ValBase b) = "ValBase " <> show b
  show (ValArray ns vs) = "ValArray " <> show ns <> " " <> show vs
  show (ValBox shapes v) = "ValBox " <> show shapes <> " " <> show v
  show ValFun {} = "ValFun <#fun>"
  show ValTFun {} = "ValTFun <#tfun>"
  show ValIFun {} = "ValIFun <#ifun>"

instance Pretty (Val m) where
  pretty (ValVar v) = pretty v
  pretty (ValBase b) = pretty b
  pretty (ValArray [] [v]) =
    pretty v
  pretty (ValArray [_] vs) =
    group $ brackets (hsep (map pretty vs))
  pretty (ValArray shape vs) =
    pretty $ splitvs shape vs
    where
      splitvs [d] vs' = ValArray [d] vs'
      splitvs (d : ds) vs' =
        ValArray [d] $ map (splitvs ds) $ split (product ds) vs'
      splitvs x y = error $ unlines [show x, show y, show (ValArray shape vs)]
  pretty (ValBox is v) =
    parens $ "box" <+> hsep (map (either pretty pretty) is) <+> pretty v
  pretty ValFun {} = "#<fun>"
  pretty ValTFun {} = "#<tfun>"
  pretty ValIFun {} = "#<ifun>"

-- | View a value as a base value, unwrapping a scalar array if needed.
asBase :: Val m -> Base
asBase (ValBase b) = b
asBase (ValArray [] [v]) = asBase v
asBase _ = error "not base"

asInt :: Val m -> Int
asInt v =
  case asBase v of
    IntVal n -> n
    b -> error $ "asInt: not an integer: " <> show b

asFloat :: Val m -> Float
asFloat v =
  case asBase v of
    FloatVal x -> x
    b -> error $ "asFloat: not a float: " <> show b

asBool :: Val m -> Bool
asBool v =
  case asBase v of
    BoolVal b -> b
    b -> error $ "asBool: not a boolean: " <> show b

valShapeOf :: Val m -> [Int]
valShapeOf (ValArray s _) = s
valShapeOf _ = mempty

-- | Lifts a scalar value into an arry.
arrayifyVal :: Val m -> Val m
arrayifyVal v@ValArray {} = v
arrayifyVal v = ValArray mempty [v]

-- | View a value as an array: its shape together with its elements.
asArray :: Val m -> ([Int], [Val m])
asArray v = case collapse v of
  ValArray shape vs -> (shape, vs)
  v' -> (mempty, [v'])

asScalar :: Val m -> Val m
asScalar v = case asArray v of
  (_, [x]) -> x
  _ -> error "asScalar: not a scalar"

-- | @split n xs@ splits @xs@ into @n@-sized chunks.
split :: Int -> [a] -> [[a]]
split _ [] = []
split n as = take n as : split n (drop n as)

-- | A flattened 'replicate'.
rep :: Int -> [a] -> [a]
rep n = concatMap $ replicate n

-- | Shape suffix subtraction (see function of the same name in the "Shape"
-- module).
(\\) :: [Int] -> [Int] -> [Int]
(\\) as bs = reverse $ prefix' (reverse as) (reverse bs)
  where
    prefix' as' [] = as'
    prefix' [] _ =
      error $
        unlines
          [ "\\ called on non sub-shape:",
            prettyString as,
            prettyString bs
          ]
    prefix' (a : as') (b : bs')
      | a == b = prefix' as' bs'
      | otherwise = a : as'

transpose :: Val m -> Val m
transpose (ValArray (m : n : ss) vss) =
  ValArray (n : m : ss) $ concat $ L.transpose $ split n vss
transpose v = v

valConcat :: Val m -> Val m
valConcat (ValArray (m : n : s) vs) =
  ValArray (m * n : s) $ concatMap (snd . asArray) vs
valConcat _ = error "concat"

collapse :: Val m -> Val m
collapse (ValArray s vs) =
  let vs' = map collapse vs
      unpack (ValArray _ vs1) = vs1
      unpack v = [v]
   in case vs' of
        ValArray s' _ : rest
          | all (\x -> valShapeOf x == s') rest ->
              ValArray (s <> s') $ concatMap unpack vs'
          | otherwise -> error $ "collapse: " <> show vs'
        _ -> ValArray s vs'
collapse v = v

valToString :: Val m -> String
valToString (ValArray _ vs) = map (toEnum . asInt) vs
valToString _ = error "valToString: not a string"
