module Interpreter.Value
  ( Val (..),
    lift,
    liftTo,
    prefix,
    split,
    rep,
    arrayifyVal,
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

-- | Values. Parameterized by a monad @m@ over which function bodies are
-- evaluated.
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

instance Show (Val m) where
  show (ValVar v) = "ValVar " <> show v
  show (ValBase b) = "ValBase " <> show b
  show (ValArray ns vs) = "ValArray " <> show ns <> " " <> show vs
  show (ValLambda pts e) = "ValLambda " <> show pts <> " " <> show e
  show (ValTLambda pts e) = "ValTLambda " <> show pts <> " " <> show e
  show (ValILambda pts e) = "ValILambda " <> show pts <> " " <> show e
  show (ValBox shapes v t) = "ValBox " <> show shapes <> " " <> show v <> " " <> show t
  show ValFun {} = "ValFun <#fun>"
  show ValTFun {} = "ValTFun <#tfun>"
  show ValIFun {} = "ValIFun <#ifun>"

instance Pretty (Val m) where
  pretty (ValVar v) = pretty v
  pretty (ValBase b) = pretty b
  pretty (ValArray [] [v]) =
    pretty v
  pretty v@(ValArray shape vs) =
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

arrayifyVal :: Val m -> Val m
arrayifyVal v@ValArray {} = v
arrayifyVal v = ValArray mempty [v]

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
