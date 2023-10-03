{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Resolution.Cache.Utils where

import Control.Exception (try)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadState, gets, modify)
import Control.Monad.Reader (MonadIO (..), MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Binary (Binary, decodeFileOrFail, encodeFile)
import qualified Data.Cache.LRU as LRU
import GTD.Configuration (GTDConfiguration)
import GTD.Utils (encodeWithTmp1, logDebugNSS)
import Text.Printf (printf)

pathAsFile :: FilePath -> FilePath
pathAsFile = fmap $ \s -> if s == '/' then '_' else s

binaryGet :: FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m, Binary a) => m (Maybe a)
binaryGet p =
  liftIO (try $ decodeFileOrFail p) >>= \case
    Left (e :: IOError) -> logDebugNSS "binary get" (printf "%s failed: %s" p (show e)) >> return Nothing
    Right ew -> case ew of
      Left (_, e) -> logDebugNSS "binary get" (printf "%s: reading succeeded, yet decodeFileOrFail failed: %s" p $ show e) >> return Nothing
      Right w -> logDebugNSS "binary get" (printf "%s succeded" p) >> return (Just w)

binaryPut :: (MonadIO m, MonadBaseControl IO m, Binary a) => FilePath -> a -> m ()
binaryPut = encodeWithTmp1 encodeFile

---

setCacheMaxSize :: (MonadLoggerIO m, MonadState (LRU.LRU k v) m, Ord k) => Integer -> m ()
setCacheMaxSize n = do
  z <- gets LRU.maxSize
  case z of
    Just n0 | n0 >= n -> return ()
    _ -> do
      logDebugNSS "package cache" $ printf "setting size to %d" n
      modify $ \lru -> LRU.fromList (Just n) $ LRU.toList lru