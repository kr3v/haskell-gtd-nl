{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Utils where

import Control.Concurrent (myThreadId)
import Control.Exception (catch, throwIO)
import Control.Lens (Lens', use, (.=))
import Control.Monad.Except (ExceptT, MonadIO (liftIO), when)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logDebugNS, logErrorNS)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans.Except (catchE, mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)
import GHC.Stats (RTSStats(..), getRTSStats, getRTSStatsEnabled)
import Numeric ( showFFloat )

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
modifyM f = StateT $ \s -> do
  s' <- f s
  return ((), s')

modifyMS :: (MonadState s m) => (s -> m s) -> m ()
modifyMS f = do
  s0 <- get
  s1 <- f s0
  put s1

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e


stats :: IO ()
stats = do
  statsE <- liftIO getRTSStatsEnabled
  when statsE $ do
    let showF2 :: Float -> String = flip (showFFloat (Just 2)) ""
    stats@RTSStats
      { init_cpu_ns = ic,
        init_elapsed_ns = ie,
        mutator_cpu_ns = mc,
        mutator_elapsed_ns = me,
        gc_cpu_ns = gc,
        gc_elapsed_ns = ge,
        cpu_ns = c,
        elapsed_ns = e
      } <-
      liftIO getRTSStats
    let cI = fromIntegral c
    let eI = fromIntegral e
    liftIO $ putStrLn $ printf "CPU : i:%s m:%s g:%s r:%s" (showF2 $ fromIntegral ic / cI) (showF2 $ fromIntegral mc / cI) (showF2 $ fromIntegral gc / cI) (showF2 $ fromIntegral (c - ic - mc - gc) / cI)
    liftIO $ putStrLn $ printf "Wall: i:%s m:%s g:%s r:%s" (showF2 $ fromIntegral ie / eI) (showF2 $ fromIntegral me / eI) (showF2 $ fromIntegral ge / eI) (showF2 $ fromIntegral (e - ie - me - ge) / eI)
    liftIO $ putStrLn $ "GCs: " ++ show stats