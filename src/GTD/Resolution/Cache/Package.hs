{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Resolution.Cache.Package (get, put, remove, exists, getS) where

import Control.Lens (over, view)
import Control.Monad (when)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Logger (LogLevel (LevelDebug), MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), asks)
import qualified Data.Aeson as JSON
import qualified Data.Binary as Binary (Binary, encodeFile)
import qualified Data.Cache.LRU as LRU
import qualified Data.HashMap.Strict as HMap
import Data.Maybe (isJust)
import qualified GTD.Cabal.Types as Cabal (Package (..), PackageWithResolvedDependencies, dKey, key, pKey)
import GTD.Configuration (Args (..), GTDConfiguration (..))
import qualified GTD.Resolution.Cache.Resolution as Resolution
import GTD.Resolution.Cache.Utils (binaryGet, pathAsFile)
import GTD.Resolution.Types (Package (..))
import qualified GTD.Resolution.Types as Package
import GTD.State (Context, cExports)
import GTD.Utils (encodeWithTmp, logDebugNSS, removeIfExistsL)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

exportsN :: String
exportsN = "exports.binary"

modulesN :: String
modulesN = "modules.binary"

path :: Cabal.Package a -> FilePath -> (MonadIO m, MonadReader GTDConfiguration m) => m FilePath
path cPkg f = do
  c <- asks _cache
  d <- Resolution.__dir $ Cabal.key cPkg
  let r = pathAsFile $ Cabal._root cPkg
      p = Cabal.dKey . Cabal._designation $ cPkg
  return $ c </> d </> (r ++ ":" ++ p ++ ":" ++ f)

__pGet :: Cabal.Package b -> FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m, Binary.Binary a) => m (Maybe a)
__pGet cPkg f = do
  p <- path cPkg f
  binaryGet p

exists :: Cabal.Package a -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
exists cPkg = do
  p <- path cPkg exportsN
  r <- liftIO $ doesFileExist p
  logDebugNSS "package cached exists" $ printf "%s, %s -> %s" (show $ Cabal.pKey . Cabal.key $ cPkg) p (show r)
  return r

get :: Cabal.PackageWithResolvedDependencies -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Package)
get cPkg = do
  modulesJ <- __pGet cPkg modulesN
  exportsJ <- __pGet cPkg exportsN
  let p = Package cPkg <$> modulesJ <*> exportsJ <*> pure HMap.empty
  logDebugNSS "package cached get" $ printf "%s -> %s (%s, %s)" (show $ Cabal.pKey . Cabal.key $ cPkg) (show $ isJust p) (show $ isJust modulesJ) (show $ isJust exportsJ)
  return p

put ::
  Cabal.PackageWithResolvedDependencies ->
  Package ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
put cPkg pkg = do
  ll <- asks $ _logLevel . _args
  modulesP <- path cPkg modulesN
  exportsP <- path cPkg exportsN
  liftIO $ encodeWithTmp Binary.encodeFile modulesP (Package._modules pkg)
  liftIO $ encodeWithTmp Binary.encodeFile exportsP (Package._exports pkg)
  liftIO $ when (ll == LevelDebug) $ do
    encodeWithTmp JSON.encodeFile (modulesP ++ ".json") (Package._modules pkg)
    encodeWithTmp JSON.encodeFile (exportsP ++ ".json") (Package._exports pkg)
  logDebugNSS "package cached put" $ printf "%s -> (%d, %d)" (show $ Cabal.pKey . Cabal.key $ cPkg) (length $ Package._modules pkg) (length $ Package._exports pkg)

remove ::
  Cabal.Package a ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
remove cPkg = do
  path cPkg modulesN >>= removeIfExistsL
  path cPkg exportsN >>= removeIfExistsL
  logDebugNSS "package cached remove" $ printf "%s" (show $ Cabal.key cPkg)

getS ::
  Context ->
  Cabal.PackageWithResolvedDependencies ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Package, Context -> Context)
getS c cPkg = do
  let k = Cabal.key cPkg
      (_, r) = LRU.lookup k (view cExports c)
      defM = over cExports (fst . LRU.lookup k)
  logDebugNSS "package cached get'" $ printf "%s -> %s" (show $ Cabal.pKey . Cabal.key $ cPkg) (show $ isJust r)
  let p = Package cPkg mempty mempty mempty
  case r of
    Just es -> return (Just p {Package._exports = es}, defM)
    Nothing -> do
      eM <- __pGet cPkg exportsN
      return $ case eM of
        Nothing -> (Nothing, defM)
        Just e -> (Just p {Package._exports = e}, over cExports (LRU.insert k e))
