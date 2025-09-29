module Interpreter.Value (Val (..)) where

import Prettyprinter
import Syntax hiding (Atom, Exp, Shape, Type)
import Syntax qualified
import VName

type Exp = Syntax.Exp Typed VName

type Atom = Syntax.Atom Typed VName

type Shape = Syntax.Shape VName

type Type = Syntax.Type VName

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
  | ValTLambda [(VName, Kind)] Exp
  | ValILambda [(VName, Sort)] Exp
  | ValBox [Shape] (Val m) Type
  | ValFun ([Val m] -> m (Val m))
  | ValTFun ([Type] -> m (Val m))
  | ValIFun ([[Int]] -> m (Val m))

instance Show (Val m)

instance Pretty (Val m) where
  pretty (ValVar v) = pretty v
  pretty (ValBase b) = pretty b
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
              map
                ( \(v, k) ->
                    tupled $ [pretty v, pretty k]
                )
                pts
     in parens $ "Tλ" <+> pArgs <+> pretty e
  pretty (ValILambda pts e) =
    let pArgs =
          parens $
            hsep $
              map
                ( \(v, s) ->
                    tupled $ [pretty v, pretty s]
                )
                pts
     in parens $ "Iλ" <+> pArgs <+> pretty e
  pretty (ValBox is v t) =
    parens $ "box" <+> hsep (map pretty is) <+> pretty v <+> pretty t
  pretty ValFun {} = "#<fun>"
  pretty ValTFun {} = "#<tfun>"
  pretty ValIFun {} = "#<ifun>"
