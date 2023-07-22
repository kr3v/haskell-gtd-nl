{-# LANGUAGE RankNTypes #-}

module GTD.Utils where

import Control.Lens (Lens', use, (.=))
import Control.Monad.Except (ExceptT, MonadIO (liftIO))
import Control.Monad.Logger (MonadLoggerIO, logDebugNS, logErrorNS, MonadLogger)
import Control.Monad.RWS (MonadState)
import Control.Monad.State (StateT (..))
import Control.Monad.Trans.Except (catchE, mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent (myThreadId)
import Text.Printf (printf)

maybeToMaybeT :: Monad m => Maybe a -> MaybeT m a
maybeToMaybeT = MaybeT . return

ultraZoom :: (MonadState s m) => Lens' s a -> StateT a m b -> m b
ultraZoom l sa = do
  a <- use l
  (b, a') <- runStateT sa a
  l .= a'
  return b

logDebugNSS :: MonadLoggerIO m => String -> String -> m ()
logDebugNSS a b = do
  now <- liftIO getPOSIXTime
  threadID <- liftIO myThreadId
  logDebugNS (T.pack a) (T.pack $ printf "%s (thread id=%s): %s" (show now) (show threadID) b)


logDebugNSS' :: (MonadIO m, MonadLogger m) => String -> String -> m ()
logDebugNSS' a b = do
  now <- liftIO getPOSIXTime
  threadID <- liftIO myThreadId
  logDebugNS (T.pack a) (T.pack $ printf "%s (thread id=%s): %s" (show now) (show threadID) b)

logErrorNSS :: MonadLoggerIO m => String -> String -> m ()
logErrorNSS a b = do
  now <- liftIO getPOSIXTime
  threadID <- liftIO myThreadId
  logErrorNS (T.pack a) (T.pack $ printf "%s (thread id=%s): %s" (show now) (show threadID) b)


tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (fmap Right m) (return . Left)

mapFrom :: Ord k => (a -> k) -> [a] -> Map.Map k a
mapFrom f xs = Map.fromList $ (\x -> (f x, x)) <$> xs

deduplicateBy :: Ord k => (a -> k) -> [a] -> [a]
deduplicateBy f xs = Map.elems $ Map.fromList $ (\x -> (f x, x)) <$> xs

deduplicate :: Ord k => [k] -> [k]
deduplicate = deduplicateBy id

withExceptT :: (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

peekM :: Monad m => (a -> m b) -> m a -> m a
peekM a m = do
  r <- m
  _ <- a r
  return r

modifyM :: (Monad m) => (s -> m s) -> StateT s m ()
modifyM f = StateT $ \ s -> do
    s' <- f s
    return ((), s')
    