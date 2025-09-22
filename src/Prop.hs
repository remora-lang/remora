module Prop where

import SymTable
import Syntax

class HasType v x where
  typeOf :: (MonadSymTable m v (Type v)) => x -> m (Type v)

instance HasType v Base where
  typeOf BoolVal {} = pure Bool
  typeOf IntVal {} = pure Int
  typeOf FloatVal {} = pure Float

-- instance HasType v (Atom v) where
--   typeOf (Base b _) = typeOf b
--   typeOf (Lambda ps e _) =
--     map snd ps :-> typeOf e
--   typeOf (TLambda ps e _) =
--     Forall ps $ typeOf e

--
-- instance HasType v (Exp v)
