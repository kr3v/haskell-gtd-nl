{-# LANGUAGE RankNTypes #-}

module GTD.Utils where

import Control.Lens (Lens', use, (.=))
import Control.Monad.RWS (MonadState)
import Control.Monad.State (MonadState, StateT (runStateT))
import Control.Monad.Trans.Maybe (MaybeT (..))

maybeToMaybeT :: Monad m => Maybe a -> MaybeT m a
maybeToMaybeT = MaybeT . return

ultraZoom :: (MonadState s m) => Lens' s a -> StateT a m b -> m b
ultraZoom l sa = do
  a <- use l
  (b, a') <- runStateT sa a
  l .= a'
  return b