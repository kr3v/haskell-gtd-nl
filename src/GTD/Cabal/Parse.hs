{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Cabal.Parse where

import Control.Monad (forM_)
import Control.Monad.Logger (MonadLoggerIO (..))
import Control.Monad.RWS (MonadReader (..), MonadWriter (..), asks)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Writer (execWriter, execWriterT)
import Data.Binary (encodeFile)
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Set as Set
import Distribution.Package (PackageIdentifier (..), packageName, unPackageName)
import Distribution.PackageDescription (BuildInfo (..), LibraryName (..), unUnqualComponentName)
import qualified Distribution.PackageDescription as Cabal (BuildInfo (..), Dependency (..), Executable (..), Library (..), PackageDescription (..), explicitLibModules, unPackageName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Utils.Path (getSymbolicPath)
import GTD.Cabal.Types (Dependency (..), Designation (Designation, _desName, _desType), DesignationType (Executable, Library), Package (..), PackageModules (..), PackageWithUnresolvedDependencies, emptyPackageModules)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.Caching.Utils (binaryGet, pathAsFile)
import GTD.Utils (logDebugNSS, removeIfExistsL)
import System.FilePath (takeDirectory, (</>), normalise)
import System.IO (IOMode (ReadMode), hGetContents, openFile)

---

__read'cache'get :: FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe [PackageWithUnresolvedDependencies])
__read'cache'get = binaryGet

__read'cache'put :: FilePath -> [PackageWithUnresolvedDependencies] -> (MonadIO m) => m ()
__read'cache'put p r = liftIO $ encodeFile p r

parse :: FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m [PackageWithUnresolvedDependencies]
parse p = do
  c <- asks _cache
  let pc = c </> pathAsFile p
  __read'cache'get pc >>= \case
    Just r -> return r
    Nothing -> do
      r <- __read'direct p
      __read'cache'put pc r
      return r

remove :: FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
remove p = do
  c <- asks _cache
  let pc = c </> pathAsFile p
  removeIfExistsL pc

__read'packageDescription :: FilePath -> (MonadLoggerIO m) => m Cabal.PackageDescription
__read'packageDescription p = do
  handle <- liftIO $ openFile p ReadMode
  (warnings, epkg) <- liftIO $ runParseResult . parseGenericPackageDescription . BS.fromString <$> hGetContents handle
  forM_ warnings (\w -> logDebugNSS "cabal read" $ "got warnings for `" ++ p ++ "`: " ++ show w)
  liftIO $ either (fail . show) (return . flattenPackageDescription) epkg

__read'direct :: FilePath -> (MonadLoggerIO m) => m [PackageWithUnresolvedDependencies]
__read'direct p = do
  logDebugNSS "cabal read" p
  pd <- __read'packageDescription p

  execWriterT $ do
    -- TODO: benchmarks, test suites
    let p0 =
          Package
            { _name = unPackageName $ packageName pd,
              _version = pkgVersion $ Cabal.package pd,
              _root = normalise $ takeDirectory p,
              _path = p,
              _designation = Designation {_desType = Library, _desName = Nothing},
              _modules = emptyPackageModules,
              _dependencies = []
            }
    let lh lib = tell . pure $ do
          p0
            { _designation = Designation {_desType = Library, _desName = libraryNameToDesignationName $ Cabal.libName lib},
              _modules = __exportsL lib,
              _dependencies = __depsU $ Cabal.libBuildInfo lib
            }
    forM_ (Cabal.library pd) lh
    forM_ (Cabal.subLibraries pd) lh
    forM_ (Cabal.executables pd) $ \exe -> tell . pure $ do
      p0
        { _designation = Designation {_desType = Executable, _desName = Just $ unUnqualComponentName $ Cabal.exeName exe},
          _modules = __exportsE exe,
          _dependencies = __depsU $ Cabal.buildInfo exe
        }

---

__exportsL :: Cabal.Library -> PackageModules
__exportsL lib =
  PackageModules
    { _srcDirs = getSymbolicPath <$> (hsSourceDirs . Cabal.libBuildInfo) lib,
      _exports = Set.fromList $ prettyShow <$> Cabal.exposedModules lib,
      _reExports = Set.fromList $ prettyShow <$> Cabal.reexportedModules lib,
      _allKnownModules = Set.fromList $ prettyShow <$> Cabal.explicitLibModules lib
    }

__exportsE :: Cabal.Executable -> PackageModules
__exportsE exe =
  PackageModules
    { _srcDirs = getSymbolicPath <$> (hsSourceDirs . Cabal.buildInfo) exe,
      _exports = Set.singleton "Main",
      _reExports = Set.empty,
      _allKnownModules = Set.fromList $ "Main" : (prettyShow <$> Cabal.otherModules (Cabal.buildInfo exe))
    }

libraryNameToDesignationName :: LibraryName -> Maybe String
libraryNameToDesignationName LMainLibName = Nothing
libraryNameToDesignationName (LSubLibName n) = Just $ unUnqualComponentName n

__depsU :: BuildInfo -> [Dependency]
__depsU i = execWriter $
  forM_ (targetBuildDepends i) $ \(Cabal.Dependency p vP ns) ->
    forM_ ns \n ->
      tell $ pure $ Dependency {_dName = Cabal.unPackageName p, _dVersion = vP, _dSubname = libraryNameToDesignationName n}