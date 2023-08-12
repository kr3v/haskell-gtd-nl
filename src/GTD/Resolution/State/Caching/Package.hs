{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Resolution.State.Caching.Package where

import Control.Exception (try)
import Control.Lens (over, view)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..), gets, modify)
import Data.Aeson (FromJSON, decode, encode)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Cache.LRU as LRU
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context, Package (..), cExports)
import qualified GTD.Resolution.State as Package
import GTD.Utils (logDebugNSS, removeIfExists)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import Text.Printf (printf)

__pGet :: Cabal.PackageFull -> FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m, FromJSON a) => m (Maybe a)
__pGet cPkg f = do
  let p = (Cabal._path . Cabal._fpackage $ cPkg) </> f
  rJ :: Either IOError (Maybe a) <- liftIO (try $ decode <$> BS.readFile p)
  case rJ of
    Left e -> do logDebugNSS "persistence get" $ printf "%s / %s failed: %s" (show $ Cabal.nameVersionF cPkg) p (show e)
    Right _ -> do logDebugNSS "persistence get" $ printf "%s / %s -> True" (show $ Cabal.nameVersionF cPkg) p
  return $ fromRight Nothing rJ

pExists :: Cabal.Package -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
pExists cPkg = do
  let p = Cabal._path cPkg </> "exports.json"
  r <- liftIO $ doesFileExist p
  logDebugNSS "package cached exists" $ printf "%s, %s -> %s" (show $ Cabal.nameVersionP cPkg) p (show r)
  return r

pGet :: Cabal.PackageFull -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Package)
pGet cPkg = do
  modulesJ <- __pGet cPkg "modules.json"
  exportsJ <- __pGet cPkg "exports.json"
  let p = Package cPkg <$> modulesJ <*> exportsJ
  logDebugNSS "package cached get" $ printf "%s -> %s" (show $ Cabal.nameVersionF cPkg) (show $ isJust p)
  return p

pStore ::
  Cabal.PackageFull ->
  Package ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
pStore cPkg pkg = do
  let root = Cabal._path . Cabal._fpackage $ cPkg
      modulesP = root </> "modules.json"
      exportsP = root </> "exports.json"
  liftIO $ BS.writeFile modulesP $ encode $ Package._modules pkg
  liftIO $ BS.writeFile exportsP $ encode $ Package._exports pkg
  logDebugNSS "package cached put" $ printf "%s -> (%d, %d)" (show $ Cabal.nameVersionF cPkg) (length $ Package._modules pkg) (length $ Package._exports pkg)

pRemove ::
  Cabal.PackageFull ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
pRemove cPkg = do
  let root = Cabal._path . Cabal._fpackage $ cPkg
      modulesP = root </> "modules.json"
      exportsP = root </> "exports.json"
  liftIO $ removeIfExists modulesP
  liftIO $ removeIfExists exportsP
  logDebugNSS "package cached remove" $ printf "%s" (show $ Cabal.nameVersionF cPkg)

---

get ::
  Context ->
  Cabal.PackageFull ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Package, Context -> Context)
get c cPkg = do
  let k = Cabal.nameVersionF cPkg
      (_, r) = LRU.lookup k (view cExports c)
      defM = over cExports (fst . LRU.lookup k)
  logDebugNSS "package cached get'" $ printf "%s -> %s" (show $ Cabal.nameVersionF cPkg) (show $ isJust r)
  case r of
    Just es -> return (Just Package {_cabalPackage = cPkg, _modules = Map.empty, Package._exports = es}, defM)
    Nothing -> do
      eM <- __pGet cPkg "exports.json"
      return $ case eM of
        Nothing -> (Nothing, defM)
        Just e -> (Just Package {_cabalPackage = cPkg, _modules = Map.empty, Package._exports = e}, over cExports (LRU.insert k e))

setCacheMaxSize :: (MonadLoggerIO m, MonadState (LRU.LRU k v) m, Ord k) => Integer -> m ()
setCacheMaxSize n = do
  z <- gets LRU.maxSize
  case z of
    Just n0 | n0 >= n -> return ()
    _ -> do
      logDebugNSS "package cache" $ printf "setting size to %d" n
      modify $ \lru -> LRU.fromList (Just n) $ LRU.toList lru
