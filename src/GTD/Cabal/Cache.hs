{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Cabal.Cache where

import Control.Exception (try)
import Control.Lens (use, (.=))
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..), asks)
import Data.Aeson (decodeFileStrict, encodeFile)
import GTD.Cabal.Parse (remove)
import GTD.Cabal.Types (Package (..), changed, vs)
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.State (Context, ccGet)
import GTD.Utils (encodeWithTmp, logDebugNSS, logErrorNSS)
import Text.Printf (printf)

load :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
load = do
  cfgP <- asks _ccGetPath
  logDebugNSS "CabalCache.load" $ printf "readFile %s" cfgP
  liftIO (try $ decodeFileStrict cfgP) >>= \case
    Left (e :: IOError) -> logErrorNSS "CabalCache.load" $ printf "readFile %s -> %s" cfgP (show e)
    Right c -> do
      forM_ c (ccGet .=)
      (ccGet . changed) .= False
      g <- length <$> use (ccGet . vs)
      logDebugNSS "CabalCache.load" $ printf "readFile succeeded %s (%d)" cfgP g

store :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
store = do
  cfgP <- asks _ccGetPath
  cc <- use ccGet
  logDebugNSS "CabalCache.store" $ printf "writeFile %s..." cfgP
  liftIO (try $ encodeWithTmp encodeFile cfgP cc) >>= \case
    Left (e :: IOError) -> logErrorNSS "CabalCache.store" $ printf "writeFile %s -> %s" cfgP (show e)
    Right _ -> logDebugNSS "CabalCache.store" $ printf "writeFile succeeded %s" cfgP

---

dropCache :: Cabal.Package a -> (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
dropCache p = remove $ _path p