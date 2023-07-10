{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GTD.Resolution.State.Caching.Package where

import Control.Exception (try)
import Control.Lens (At (at), use, (%=))
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..))
import Data.Aeson (FromJSON, decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Either (fromRight)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context, Package (..), cExports)
import GTD.Utils (logDebugNSS)
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import Text.Printf (printf)
import qualified GTD.Resolution.State as Package

persistenceGet :: Cabal.PackageFull -> FilePath -> (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m, FromJSON a) => m (Maybe a)
persistenceGet cPkg f = do
  let p = (Cabal._path . Cabal._fpackage $ cPkg) </> f
  rJ :: Either IOError (Maybe a) <- liftIO (try $ decode <$> BS.readFile p)
  let r = fromRight Nothing rJ
  logDebugNSS "persistence get" $ printf "%s / %s -> %s" (show $ Cabal.nameVersionF cPkg) p (show $ isJust r)
  return r

persistenceExists :: Cabal.Package -> (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m Bool
persistenceExists cPkg = do
  let p = Cabal._path cPkg </> "exports.json"
  r <- liftIO $ doesFileExist p
  logDebugNSS "package cached exists" $ printf "%s, %s -> %s" (show $ Cabal.nameVersionP cPkg) p (show r)
  return r

packagePersistenceGet ::
  Cabal.PackageFull ->
  (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m (Maybe Package)
packagePersistenceGet cPkg = do
  modulesJ <- persistenceGet cPkg "modules.json"
  exportsJ <- persistenceGet cPkg "exports.json"
  let p = Package cPkg <$> modulesJ <*> exportsJ
  logDebugNSS "package cached get" $ printf "%s -> %s" (show $ Cabal.nameVersionF cPkg) (show $ isJust p)
  return p

packageCachedPut ::
  Cabal.PackageFull ->
  Package ->
  (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m ()
packageCachedPut cPkg pkg = do
  let root = Cabal._path . Cabal._fpackage $ cPkg
      modulesP = root </> "modules.json"
      exportsP = root </> "exports.json"
  liftIO $ BS.writeFile modulesP $ encode $ Package._modules pkg
  liftIO $ BS.writeFile exportsP $ encode $ Package._exports pkg
  logDebugNSS "package cached put" $ printf "%s -> (%d, %d)" (show $ Cabal.nameVersionF cPkg) (length $ Package._modules pkg) (length $ Package._exports pkg)

packageCachedGet ::
  Cabal.PackageFull ->
  (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m (Maybe Package)
packageCachedGet cPkg = do
  let k = Cabal.nameVersionF cPkg
  c <- use $ cExports . at k
  case c of
    Just es -> return $ Just Package {_cabalPackage = cPkg, _modules = Map.empty, Package._exports = es}
    Nothing -> do
      eM <- persistenceGet cPkg "exports.json"
      case eM of
        Nothing -> return Nothing
        Just e -> do
          cExports %= Map.insert k e
          return $ Just Package {_cabalPackage = cPkg, _modules = Map.empty, Package._exports = e}
