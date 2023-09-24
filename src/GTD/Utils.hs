{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Utils where

import Control.Concurrent (MVar, myThreadId, newMVar, withMVar)
import Control.Exception (IOException, throwIO, try)
import Control.Exception.Lifted (catch)
import Control.Lens (Lens', use, (.=))
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadIO (..), forM, when)
import Control.Monad.Logger (Loc, LogLevel (..), LogSource, LogStr, LoggingT (..), MonadLogger, MonadLoggerIO, defaultLogStr, filterLogger, fromLogStr, logDebugNS, logErrorNS, logOtherNS)
import qualified Control.Monad.Logger as Logger
import Control.Monad.RWS (MonadState (..))
import Control.Monad.State (StateT (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (catchE, mapExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S8
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import GHC.Stats (RTSStats (..), getRTSStats, getRTSStatsEnabled)
import Numeric (showFFloat)
import System.Directory (removeFile, renameFile)
import System.IO (BufferMode (..), Handle, IOMode (..), hSetBuffering, withFile)
import System.IO.Error (isDoesNotExistError)
import System.Random (randomIO)
import Text.Printf (printf)

maybeToMaybeT :: Monad m => Maybe a -> MaybeT m a
maybeToMaybeT = MaybeT . return

ultraZoom :: (MonadState s m) => Lens' s a -> StateT a m b -> m b
ultraZoom l sa = do
  a <- use l
  (b, a') <- runStateT sa a
  l .= a'
  return b

overM :: (MonadState s m) => Lens' s a -> (a -> m a) -> m ()
overM l f = do
  a <- use l
  a' <- f a
  l .= a'

modifyEachM :: (MonadState s m, Traversable t) => Lens' s (t a) -> (a -> m a) -> m ()
modifyEachM l f = do
  ta <- use l
  tb <- traverse f ta
  l .= tb

logDebugNSS :: MonadLoggerIO m => String -> String -> m ()
logDebugNSS a b = do
  now <- liftIO getCurrentTime
  threadID <- liftIO myThreadId
  logDebugNS (T.pack a) (T.pack $ printf "%s (thread id=%s): %s" (iso8601Show now) (show threadID) b)

logDebugNSS' :: (MonadIO m, MonadLogger m) => String -> String -> m ()
logDebugNSS' a b = do
  now <- liftIO getCurrentTime
  threadID <- liftIO myThreadId
  logDebugNS (T.pack a) (T.pack $ printf "%s (thread id=%s): %s" (iso8601Show now) (show threadID) b)

logErrorNSS :: MonadLoggerIO m => String -> String -> m ()
logErrorNSS a b = do
  now <- liftIO getCurrentTime
  threadID <- liftIO myThreadId
  logErrorNS (T.pack a) (T.pack $ printf "%s (thread id=%s): %s" (iso8601Show now) (show threadID) b)

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
removeIfExists n = removeFile n `catch` handleExists
  where
    handleExists e
      | isDoesNotExistError e = return ()
      | otherwise = throwIO e

removeIfExistsL :: FilePath -> (MonadLoggerIO m) => m ()
removeIfExistsL n = do
  x :: Either IOError () <- liftIO $ try $ removeFile n
  logErrorNSS "removeIfExistsL" $ case x of
    Left e -> printf "%s -> %s" n (show e)
    Right _ -> printf "%s -> success" n

stats :: IO ()
stats = do
  statsE <- liftIO getRTSStatsEnabled
  when statsE $ do
    let showF2 :: Float -> String = flip (showFFloat (Just 2)) ""
    s@RTSStats
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
    liftIO $ putStrLn $ "GCs: " ++ show s

getTotalMemory :: IO Int
getTotalMemory = do
  content <- readFile "/proc/meminfo"
  let line [name, val, _] = Just (name, read val :: Int)
      line [name, val] = Just (name, read val :: Int)
      line _ = Nothing
      memInfo = mapMaybe (line . words) $ lines content
      lookupVal name = fromMaybe 0 (lookup name memInfo)
      total = lookupVal "MemTotal:"
  return (total `div` 1024)

getUsableFreeMemory :: IO Int
getUsableFreeMemory = do
  content <- readFile "/proc/meminfo"
  let line [name, val, _] = Just (name, read val :: Int)
      line [name, val] = Just (name, read val :: Int)
      line _ = Nothing
      memInfo = mapMaybe (line . words) $ lines content
      lookupVal name = fromMaybe 0 (lookup name memInfo)
      freeMem = lookupVal "MemFree:"
      buffers = lookupVal "Buffers:"
      cached = lookupVal "Cached:"
  return ((freeMem + buffers + cached) `div` 1024)

concatMapM :: (Traversable t, Monad f) => (a -> f [b]) -> t a -> f [b]
concatMapM a b = concat <$> mapM a b

concatForM :: (Traversable t, Monad f) => t a -> (a -> f [b]) -> f [b]
concatForM a b = concat <$> forM a b

storeIOExceptionToMonadError :: (MonadError String m, MonadIO m) => IO a -> m a
storeIOExceptionToMonadError a = do
  x :: Either IOException a <- liftIO $ try a
  case x of
    Left e -> throwError $ show e
    Right x -> return x

-- | Monadic generalization of 'maybe'.
maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

-- | Monadic generalization of 'fromMaybe'.
fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM n = maybeM n pure

(>==>) :: Monad m => (a -> m (b, s -> s)) -> (b -> m (c, s -> s)) -> (a -> m (c, s -> s))
f >==> g = \x -> do
  (y, m1) <- f x
  (z, m2) <- g y
  return (z, m2 . m1)

(<==<) :: Monad m => (b -> m (c, s -> s)) -> (a -> m (b, s -> s)) -> (a -> m (c, s -> s))
(<==<) = flip (>==>)

---

type LogF = Logger.Loc -> Logger.LogSource -> Logger.LogLevel -> Logger.LogStr -> IO ()

combine :: LogF -> LogF -> LogF
combine l r loc srcT
  | src == statusS = l loc (T.pack "")
  | otherwise = r loc srcT
  where
    src = unpack srcT

statusS :: String
statusS = "status.log"

statusL :: FilePath -> LogF
statusL p a b c d = encodeWithTmp BS.writeFile p $ BS.drop 3 $ fromLogStr $ defaultLogStr a b c d

updateStatus :: String -> MonadLogger m => m ()
updateStatus s = logOtherNS (T.pack statusS) (LevelOther $ T.pack "") (T.pack s)

logOutput :: MVar Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logOutput hm loc src level msg =
  withMVar hm $ \h ->
    S8.hPutStr h $ fromLogStr $ defaultLogStr loc src level msg

withLogging :: FilePath -> FilePath -> LogLevel -> LoggingT IO r -> IO r
withLogging logP logS ll action = withFile logP AppendMode $ \h -> do
  hSetBuffering h NoBuffering
  hm <- newMVar h
  (`runLoggingT` combine (statusL logS) (logOutput hm)) $
    filterLogger (\_ l -> l >= ll) $ do
      action

---

encodeWithTmp :: (FilePath -> a -> IO ()) -> FilePath -> a -> IO ()
encodeWithTmp = encodeWithTmp2

encodeWithTmp1 :: (MonadIO m, MonadBaseControl IO m) => (FilePath -> a -> IO ()) -> FilePath -> a -> m ()
encodeWithTmp1 f = encodeWithTmp2 (\a b -> liftIO $ f a b)

encodeWithTmp2 :: (MonadIO m, MonadBaseControl IO m) => (FilePath -> a -> m ()) -> FilePath -> a -> m ()
encodeWithTmp2 f p a = x `catch` (\(e :: IOError) -> liftIO (putStrLn $ printf "encodeWithTmp(n): %s" (show e)) >> error (show e))
  where
    x = do
      y :: Int32 <- randomIO
      let pT = p ++ ".tmp." ++ show (abs y)
      f pT a
      liftIO $ renameFile pT p