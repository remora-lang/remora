module SymTable where

import Data.Map (Map)
import Data.Map qualified as M

newtype SymTable v a = SymTable {symTable :: Map v a}
  deriving
    ( Show,
      Eq,
      Ord,
      Semigroup,
      Monoid,
      Functor,
      Foldable,
      Traversable
    )

class (Monad m, Ord v) => MonadSymTable m v a where
  askSymTable :: m (SymTable v a)
  withSymTable :: (SymTable v a -> SymTable v a) -> m b -> m b

askSym :: (MonadSymTable m v a) => v -> m (Maybe a)
askSym v = ((M.!? v) . symTable) <$> askSymTable

insertSym :: (MonadSymTable m v a) => v -> a -> m b -> m b
insertSym v a = withSymTable $ SymTable . M.insert v a . symTable
