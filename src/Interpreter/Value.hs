module Interpreter.Value
  ( Val (..),
    arrayifyVal,
    arrayValView,
    split,
    rep,
    (\\),
  )
where

import Prettyprinter
import Syntax hiding (Atom, Exp, HasShape (..), IVar, Shape, TVar, Type, (\\))
import Syntax qualified
import Util
import VName

type Exp = Syntax.Exp Typed VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

type TVar = Syntax.TVar VName

type IVar = Syntax.IVar VName

-- | Values. Parameterized by a monad @m@ over which function bodies are
-- evaluated.
data Val m
  = -- | Variables.
    ValVar VName
  | -- | Base values.
    ValBase Base
  | -- | Array value.
    ValArray [Int] [Val m]
  | -- | Lambda.
    ValLambda [(VName, Type)] Exp
  | -- | Type lambda.
    ValTLambda [TVar] Exp
  | -- | Index lambda.
    ValILambda [IVar] Exp
  | -- | Box.
    ValBox [Shape] (Val m) Type
  | -- | Function.
    ValFun ([Val m] -> m (Val m))
  | -- | Type function.
    ValTFun ([Type] -> m (Val m))
  | -- | Index function.
    ValIFun ([[Int]] -> m (Val m))

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
  pretty (ValArray _ vs) =
    group $ encloseSep "[" "]" line (map pretty vs)
  pretty (ValLambda pts e) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, t) ->
                    tupled [pretty v, pretty t]
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

-- | Lifts a scalar value into an arry.
arrayifyVal :: Val m -> Val m
arrayifyVal v@ValArray {} = v
arrayifyVal v = ValArray mempty [v]

-- | An array view on a value.
arrayValView :: Val m -> (([Int], [Val m]) -> a) -> a
arrayValView (ValArray shape vs) m = m (shape, vs)
arrayValView v m = m (mempty, [v])

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
