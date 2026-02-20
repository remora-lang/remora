module Interpreter.Value
  ( Val (..),
    arrayifyVal,
    baseValView,
    baseValViews,
    arrayValView,
    arrayValViews,
    split,
    rep,
    (\\),
    transpose,
    valConcat,
    collapse,
    valShapeOf,
  )
where

import Data.List qualified as L
import Prettyprinter
import Syntax hiding (Atom, AtomType, Exp, Extent, ExtentParam, Shape, Type, TypeParam)
import Syntax qualified
import Util
import VName

type Type = Syntax.Type VName

type AtomType = Syntax.AtomType VName

type Extent = Syntax.Extent VName

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
    ValFun ([Val m] -> m (Val m))
  | -- | Type function.
    ValTFun ([Type] -> m (Val m))
  | -- | Index function.
    ValIFun ([Either Int [Int]] -> m (Val m))

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
    pretty $ splitvs (reverse shape) vs
    where
      splitvs [d] vs' = ValArray [d] vs'
      splitvs (d : ds) vs' =
        ValArray [d] $ map (splitvs ds) $ split d vs'
      splitvs x y = error $ unlines [show x, show y, show (ValArray shape vs)]
  pretty (ValBox is v) =
    parens $ "box" <+> hsep (map (either pretty pretty) is) <+> pretty v
  pretty ValFun {} = "#<fun>"
  pretty ValTFun {} = "#<tfun>"
  pretty ValIFun {} = "#<ifun>"

-- TODO: shouldn't need this
baseValView :: Val m -> (Base -> a) -> a
baseValView v m = baseValViews [v] (m . head)

baseValViews :: [Val m] -> ([Base] -> a) -> a
baseValViews vs m = m $ map unpack vs
  where
    unpack (ValBase b) = b
    unpack (ValArray [] [v]) = unpack v
    unpack _ = error "not base"

valShapeOf :: Val m -> [Int]
valShapeOf (ValArray s _) = s
valShapeOf _ = mempty

-- | Lifts a scalar value into an arry.
arrayifyVal :: Val m -> Val m
arrayifyVal v@ValArray {} = v
arrayifyVal v = ValArray mempty [v]

-- | An array view on a value.
arrayValView :: Val m -> (([Int], [Val m]) -> a) -> a
arrayValView v m = arrayValViews [v] (m . head)

-- | An array view on a values.
arrayValViews :: [Val m] -> ([([Int], [Val m])] -> a) -> a
arrayValViews vs m = m $ map unpack vs
  where
    unpack (ValArray shape vs') = (shape, vs')
    unpack v = (mempty, [v])

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
  ValArray (m * n : s) $ arrayValViews vs $ concat . map snd
valConcat _ = error "concat"

collapse :: Val m -> Val m
collapse (ValArray s vs) =
  let vs' = map collapse vs
      unpack (ValArray _ vs) = vs
      unpack v = [v]
   in case vs' of
        ValArray s' vs'' : rest
          | all (\x -> valShapeOf x == s') rest ->
              ValArray (s <> s') $ concatMap unpack vs'
          | otherwise -> error $ "collapse: " <> show vs'
        _ -> ValArray s vs'
collapse v = v
