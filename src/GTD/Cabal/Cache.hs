{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Cabal.Cache where

import Control.Exception (try)
import Control.Lens (use, (.=))
import Control.Monad (forM_)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..), asks)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import GTD.Cabal.Get (changed, vs)
import GTD.Cabal.Parse (remove)
import GTD.Cabal.Types (Package (..))
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context, ccGet)
import GTD.Utils (logDebugNSS, logErrorNSS)
import Text.Printf (printf)

load :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
load = do
  cfgP <- asks _ccGetPath
  cE <- liftIO $ try $ BS.readFile cfgP
  case cE of
    Left (e :: IOError) -> logErrorNSS "CabalCache.load" $ printf "readFile %s -> %s" cfgP (show e)
    Right c -> do
      forM_ (decode c) (ccGet .=)
      (ccGet . changed) .= False
      g <- length <$> use (ccGet . vs)
      logDebugNSS "CabalCache.load" $ printf "loaded %s (%d)" cfgP g

store :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
store = do
  cfgP <- asks _ccGetPath
  cc <- use ccGet
  o <- liftIO $ try $ BS.writeFile cfgP $ encode cc
  case o of
    Left (e :: IOError) -> logErrorNSS "CabalCache.store" $ printf "writeFile %s -> %s" cfgP (show e)
    Right _ -> logDebugNSS "CabalCache.store" $ printf "stored %s" cfgP

---

dropCache :: Cabal.Package a -> (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
dropCache p = remove $ _path p