module Binds
  ( HasBinds (..),
    emitBind,
    collectBinds,
    drainBinds,
  )
where

import Control.Monad.State
import Syntax (Bind)

class HasBinds s where
  getBinds :: s -> [Bind]
  putBinds :: [Bind] -> s -> s

emitBind :: (MonadState s m, HasBinds s) => Bind -> m ()
emitBind b = modify $ \s -> putBinds (b : getBinds s) s

collectBinds :: (MonadState s m, HasBinds s) => m a -> m (a, [Bind])
collectBinds m = do
  bs <- gets getBinds
  modify $ putBinds mempty
  x <- m
  emitted <- gets getBinds
  modify $ putBinds bs
  pure (x, reverse emitted)

drainBinds :: (MonadState s m, HasBinds s) => m [Bind]
drainBinds = do
  bs <- gets getBinds
  modify $ putBinds mempty
  pure $ reverse bs
