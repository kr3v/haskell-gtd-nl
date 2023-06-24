{-# LANGUAGE RankNTypes #-}

module GTD.Utils where

import Control.Lens (Lens', use, (.=))
import Control.Monad.Except (ExceptT, MonadIO (liftIO))
import Control.Monad.Logger (MonadLoggerIO, logDebugNS, logErrorNS)
import Control.Monad.RWS (MonadState)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans.Except (catchE, mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)

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
  logDebugNS (T.pack a) (T.pack $ show now ++ ": " ++ b)

logErrorNSS :: MonadLoggerIO m => String -> String -> m ()
logErrorNSS a b = do
  now <- liftIO getPOSIXTime
  logErrorNS (T.pack a) (T.pack $ show now ++ ": " ++ b)

tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (fmap Right m) (return . Left)

deduplicateBy :: Ord k => (a -> k) -> [a] -> [a]
deduplicateBy f xs = Map.elems $ Map.fromList $ (\x -> (f x, x)) <$> xs

withExceptT :: (Functor m) => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = mapExceptT $ fmap $ either (Left . f) Right

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)
