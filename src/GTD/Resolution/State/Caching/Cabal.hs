{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Resolution.State.Caching.Cabal where

import Control.Exception (try)
import Control.Lens (At (at), use, (%=), (.=))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError, MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..), asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context, ccFindAt, ccFull, ccGet)
import GTD.Utils (logDebugNSS, logErrorNSS, ultraZoom)
import Text.Printf (printf)

cabalFindAtCached ::
  FilePath ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m Cabal.PackageFull
cabalFindAtCached p = do
  e <- use $ ccFindAt . at p
  case e of
    Just d -> return d
    Nothing -> do
      d <- Cabal.findAt p
      dF <- ultraZoom ccGet $ Cabal.full d
      ccFindAt %= Map.insert p dF
      return dF

cabalFull ::
  Cabal.Package ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m Cabal.PackageFull
cabalFull pkg = do
  let k = Cabal.nameVersionP pkg
  e <- use $ ccFull . at k
  case e of
    Just d -> return d
    Nothing -> do
      d <- ultraZoom ccGet $ Cabal.full pkg
      ccFull %= Map.insert k d
      return d

---

cabalCacheGet :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
cabalCacheGet = do
  cfgP <- asks _ccGetPath
  cE :: Either IOError BS.ByteString <- liftIO $ try (BS.readFile cfgP)
  case cE of
    Left e -> logErrorNSS "cabalCacheGet" $ printf "readFile %s -> %s" cfgP (show e)
    Right c -> forM_ (decode c) (ccGet .=)

cabalCacheStore :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
cabalCacheStore = do
  cfgP <- asks _ccGetPath
  cc <- use ccGet
  o :: Either IOError () <- liftIO $ try $ BS.writeFile cfgP $ encode cc
  case o of
    Left e -> logErrorNSS "cabalCacheStore" $ printf "writeFile %s -> %s" cfgP (show e)
    Right _ -> return ()
