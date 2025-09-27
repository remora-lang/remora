module Substitute where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S

class Substitute a b c where
  substitute :: Map a b -> c -> c

substituteOne :: (Substitute a b c) => (a, b) -> c -> c
substituteOne (a, b) = substitute $ M.singleton a b

instance (Ord c, Substitute a b c) => Substitute a b (Set c) where
  substitute subst = S.map $ substitute subst

instance (Ord c, Substitute a b c) => Substitute a b [c] where
  substitute subst = map $ substitute subst

instance (Substitute a b c) => Substitute a b (Map k c) where
  substitute subst = fmap $ substitute subst
