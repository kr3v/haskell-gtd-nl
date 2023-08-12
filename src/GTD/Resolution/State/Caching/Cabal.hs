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
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context, ccFindAt, ccFull, ccGet)
import GTD.Utils (logErrorNSS, ultraZoom)
import Text.Printf (printf)

findAt ::
  FilePath ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m [Cabal.Package Cabal.DependenciesResolved]
findAt p = do
  e <- use $ ccFindAt . at p
  case e of
    Just d -> return d
    Nothing -> do
      d <- Cabal.findAt p
      dF <- mapM full d
      ccFindAt %= Map.insert p dF
      return dF

full ::
  Cabal.Package Cabal.DependenciesUnresolved ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m (Cabal.Package Cabal.DependenciesResolved)
full pkg = fst <$> fullD pkg

fullD ::
  Cabal.Package Cabal.DependenciesUnresolved ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m (Cabal.Package Cabal.DependenciesResolved, [Cabal.Package Cabal.DependenciesUnresolved])
fullD pkg = do
  let k = Cabal.key pkg
  e <- use $ ccFull . at k
  case e of
    Just d -> return d
    Nothing -> do
      r <- ultraZoom ccGet $ Cabal.full pkg
      ccFull %= Map.insert k r
      return r

---

load :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
load = do
  cfgP <- asks _ccGetPath
  cE :: Either IOError BS.ByteString <- liftIO $ try (BS.readFile cfgP)
  case cE of
    Left e -> logErrorNSS "cabalCacheFetch" $ printf "readFile %s -> %s" cfgP (show e)
    Right c -> forM_ (decode c) (ccGet .=)

store :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
store = do
  cfgP <- asks _ccGetPath
  cc <- use ccGet
  o :: Either IOError () <- liftIO $ try $ BS.writeFile cfgP $ encode cc
  case o of
    Left e -> logErrorNSS "cabalCacheStore" $ printf "writeFile %s -> %s" cfgP (show e)
    Right _ -> return ()
