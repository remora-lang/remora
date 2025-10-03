module Reason where

import Control.Monad
import Control.Monad.Identity
import Data.Map (Map)
import Shape
import Symbolic

-- data Constraint v
--  = LTE (Shape v) (Shape v)
--  | (:&&) (Constraint v) (Constraint v)
--  | Not (Constraint v)
--
-- deriving instance (Show v) => Show (Constraint v)
--
-- deriving instance (Eq v) => Eq (Constraint v)
--
-- (<!) :: Shape v -> Shape v -> Constraint v
-- s <! t = s <=! t :&& s /=! t
--
-- (>!) :: Shape v -> Shape v -> Constraint v
-- s >! t = Not $ s <=! t
--
-- (<=!) :: Shape v -> Shape v -> Constraint v
-- (<=!) = LTE
--
-- (>=!) :: Shape v -> Shape v -> Constraint v
-- s >=! t = Not $ s <! t
--
-- (==!) :: Shape v -> Shape v -> Constraint v
-- s ==! t = s <=! t :&& s >=! t
--
-- (/=!) :: Shape v -> Shape v -> Constraint v
-- s /=! t = Not $ s ==! t
--
-- infix 4 <!, <=!, >!, >=!, ==!, /=!
--
-- infixr 3 :&&
--
-- (<?), (<=?), (>?), (>=?), (==?), (/=?) :: Shape v -> Shape v -> Bool
--  solve :: [Constraint v] -> Maybe (Map v (Shape v))
--  s <? t = (&&) <$> (s <=? t) <*> (s /=?) t
--  s >? t = not <$> (s <=? t)
--  s >=? t = not <$> (s <? t)
--  s ==? t = (&&) <$> (s <=? t) <*> (s >=? t)
--  s /=? t = not <$> (s ==? t)
--  {-# MINIMAL (<=?), solve #-}
--
-- infix 4 <?, <=?, >?, >=?, ==?, /=?
--
-- maximumShape :: (Foldable t) => t (Shape v) -> Shape v
-- maximumShape =
--  foldr
--    ( \next shape ->
--        if shape <? next
--          then next
--          else shape
--    )
--    mempty
