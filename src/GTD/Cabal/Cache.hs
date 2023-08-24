{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Cabal.Cache where

import Control.Exception (try)
import Control.Lens (use, (.=))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError, MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..), asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import GTD.Cabal.FindAt (findAt)
import GTD.Cabal.Full (full)
import GTD.Cabal.Package (Package (..), PackageWithResolvedDependencies)
import qualified GTD.Cabal.Package as Cabal
import GTD.Cabal.Parse (remove)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context, ccGet)
import GTD.Utils (logErrorNSS)
import Text.Printf (printf)

--

findAtF ::
  FilePath ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m [PackageWithResolvedDependencies]
findAtF p = findAt p >>= mapM full

---

load :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
load = do
  cfgP <- asks _ccGetPath
  cE :: Either IOError BS.ByteString <- liftIO $ try $ BS.readFile cfgP
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

---

dropCache :: Cabal.PackageWithResolvedDependencies -> (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
dropCache p = remove $ _path p