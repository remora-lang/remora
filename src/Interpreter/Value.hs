module Interpreter.Value
  ( Val (..),
    lift,
    liftTo,
    prefix,
    split,
    rep,
  )
where

import Data.List qualified as L
import Prettyprinter
import Syntax hiding (Atom, Exp, HasShape (..), IVar, Shape, TVar, Type)
import Syntax qualified
import VName

type Exp = Syntax.Exp Typed VName

type Atom = Syntax.Atom Typed VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

type TVar = Syntax.TVar VName

type IVar = Syntax.IVar VName

-- data ValShape
--  = ValDim Int
--  | ValShape [ValShape]
--  deriving (Show, Eq)
--
-- instance Pretty ValShape where
--  pretty (ValDim d) = pretty d
--  pretty (ValShape s) = parens $ hsep ("shape" : map pretty s)
--
-- instance Semigroup ValShape where
--  s <> t = ValShape [s, t]
--
-- instance Monoid ValShape where
--  mempty = ValShape []
--
-- normValShape :: ValShape -> ValShape
-- normValShape (ValDim n) = ValDim n
-- normValShape (ValShape ss) =
--  case merged of
--    [s] -> s
--    _ -> ValShape merged
--  where
--    merged =
--      concatMap
--        ( \s ->
--            case normValShape s of
--              ValShape ss' -> ss'
--              s' -> [s']
--        )
--        ss

data Val m
  = ValVar VName
  | ValBase Base
  | ValArray [Int] [Val m]
  | ValLambda [(VName, Type)] Exp
  | ValTLambda [TVar] Exp
  | ValILambda [IVar] Exp
  | ValBox [Shape] (Val m) Type
  | ValFun ([Val m] -> m (Val m))
  | ValTFun ([Type] -> m (Val m))
  | ValIFun ([[Int]] -> m (Val m))

instance Show (Val m)

instance Pretty (Val m) where
  pretty (ValVar v) = pretty v
  pretty (ValBase b) = pretty b
  pretty (ValArray [] [v]) =
    pretty v
  pretty (ValArray shape vs) =
    group $ encloseSep "[" "]" ("," <> line) (map pretty vs)
  pretty (ValLambda pts e) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, t) ->
                    tupled $ [pretty v, pretty t]
                )
                pts
     in parens $ "λ" <+> pArgs <+> pretty e
  pretty (ValTLambda pts e) =
    let pArgs =
          parens $
            hsep $
              map pretty pts
     in parens $ "Tλ" <+> pArgs <+> pretty e
  pretty (ValILambda pts e) =
    let pArgs =
          parens $
            hsep $
              map pretty pts
     in parens $ "Iλ" <+> pArgs <+> pretty e
  pretty (ValBox is v t) =
    parens $ "box" <+> hsep (map pretty is) <+> pretty v <+> pretty t
  pretty ValFun {} = "#<fun>"
  pretty ValTFun {} = "#<tfun>"
  pretty ValIFun {} = "#<ifun>"

shapeOf :: Val m -> [Int]
shapeOf (ValArray s _) = s
shapeOf _ = mempty

lift :: [Int] -> Val m -> Val m
lift [] v = v
lift shape@(d : ds) v =
  ValArray (shape <> shapeOf v) $ replicate d elems
  where
    elems = lift ds v

liftTo :: [Int] -> Val m -> Val m
liftTo shape v = lift (take (length shape - length (shapeOf v)) shape) v

split :: Int -> [a] -> [[a]]
split _ [] = []
split n as = take n as : split n (drop n as)

rep :: Int -> [a] -> [a]
rep n = concat . replicate n

prefix :: [Int] -> [Int] -> [Int]
prefix as bs = prefix' [] as
  where
    prefix' as [] = as
    prefix' as' (a : as)
      | (a : as) `L.isPrefixOf` bs = as'
      | otherwise = prefix' (as' ++ [a]) as
