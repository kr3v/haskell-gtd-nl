{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Cabal where

import Control.Lens (makeLenses, view, use, At (..))
import Control.Monad (forM, forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), asks, MonadState)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.UTF8 as BS
import Data.List (find)
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Distribution.Compat.Prelude (Generic)
import Distribution.Package (PackageIdentifier (..), packageName)
import Distribution.PackageDescription (BuildInfo (..), Dependency (Dependency), Library (..), PackageDescription (..), allBuildInfo, unPackageName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Utils.Path (getSymbolicPath)
import GTD.Configuration (GTDConfiguration (..), repos)
import GTD.Utils (logDebugNSS, deduplicate)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>), takeDirectory)
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))
import qualified Data.Map as Map
import Control.Monad.RWS.Class (modify)

type PackageNameS = String

type ModuleNameS = String

data Package = Package
  { _path :: FilePath,
    _cabal :: PackageDescription
  }
  deriving (Show, Generic)

$(makeLenses ''Package)

nameP :: Package -> String
nameP = prettyShow . pkgName . package . _cabal

---

_read :: FilePath -> (MonadLoggerIO m) => m Package
_read p = do
  logDebugNSS "cabal read" p
  handle <- liftIO $ openFile p ReadMode
  (warnings, epkg) <- liftIO $ runParseResult . parseGenericPackageDescription . BS.fromString <$> hGetContents handle
  forM_ warnings (\w -> logDebugNSS "cabal read" $ "got warnings for `" ++ p ++ "`: " ++ show w)
  pd <- liftIO $ either (fail . show) (return . flattenPackageDescription) epkg
  return $ Package (takeDirectory p) pd

findAt ::
  FilePath ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadError String m) => m Package
findAt wd = do
  cabalFiles <- liftIO $ filter (\x -> takeExtension x == ".cabal") <$> listDirectory wd
  cabalFile <- case length cabalFiles of
    0 -> throwError "No cabal file found"
    1 -> return $ wd </> head cabalFiles
    _ -> throwError "Multiple cabal files found"
  logDebugNSS "definition" $ "Found cabal file: " ++ cabalFile
  _read cabalFile

---

type GetCache = Map.Map String (Maybe FilePath)

-- executes `cabal get` on given `pkg + pkgVerPredicate`
get :: String -> String -> (MonadLoggerIO m, MonadState GetCache m, MonadReader GTDConfiguration m) => MaybeT m FilePath
get pkg pkgVerPredicate = do
  let k = pkg ++ pkgVerPredicate
  r0 <- use $ at k
  case r0 of
    Just p -> MaybeT $ return p
    Nothing -> do
      reposR <- view repos
      (_, Just hout, Just herr, h) <- liftIO $ createProcess (proc "cabal" ["get", k, "--destdir", reposR]) {std_out = CreatePipe, std_err = CreatePipe}
      stdout <- liftIO $ hGetContents hout
      stderr <- liftIO $ hGetContents herr
      let content = stdout ++ stderr
      let re = pkg ++ "-" ++ "[^\\/]*\\/"
      let packageVersion :: [String] = (=~ re) <$> lines content
      ec <- liftIO $ waitForProcess h
      logDebugNSS "cabal get" $ printf "cabal get %s %s: exit code %s" pkg pkgVerPredicate (show ec)

      let r = find (not . null) packageVersion
      modify $ Map.insert k r
      MaybeT $ return r

---

data PackageModules = PackageModules
  { _srcDirs :: [FilePath],
    _exports :: Set.Set ModuleNameS
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''PackageModules)

emptyPackageModules :: PackageModules
emptyPackageModules = PackageModules [] Set.empty

__exports :: PackageDescription -> Maybe PackageModules
__exports pkg = do
  lib <- library pkg
  let es = Set.fromList $ prettyShow <$> exposedModules lib
  let srcDirs = getSymbolicPath <$> (hsSourceDirs . libBuildInfo) lib
  return $ PackageModules srcDirs es

---

__dependencies :: Package -> (MonadLoggerIO m, MonadState GetCache m, MonadReader GTDConfiguration m) => m [Package]
__dependencies pkg = do
  let c = view cabal pkg
  logDebugNSS "cabal fetch" (prettyShow $ packageName c)
  let deps = concatMap targetBuildDepends $ allBuildInfo c
  let names = deduplicate $ (\(Dependency n v _) -> (unPackageName n, prettyShow v)) <$> deps
  logDebugNSS "cabal fetch" $ "dependencies: " ++ show names
  pkgs <-
    let fetchDependency n vp = (,) n <$> get n vp
        fetchDependencies ds = catMaybes <$> mapM runMaybeT (uncurry fetchDependency <$> ds)
     in fetchDependencies names
  reposR <- asks _repos
  forM pkgs $ \(n, p) -> _read $ reposR </> p </> (n ++ ".cabal")

---

data PackageFull = PackageFull
  { _fpackage :: Package,
    _modules :: PackageModules,
    _dependencies :: [Package]
  }
  deriving (Show, Generic)

$(makeLenses ''PackageFull)

full :: Package -> (MonadLoggerIO m, MonadState GetCache m, MonadReader GTDConfiguration m) => m PackageFull
full pkg = do
  logDebugNSS "Cabal.full" $ "enriching " ++ nameP pkg
  deps <- __dependencies pkg
  let ms = fromMaybe emptyPackageModules $ __exports . _cabal $ pkg
  return $ PackageFull pkg ms deps

---

data PackageWithVersion = PackageWithVersion
  { _name :: PackageNameS,
    _version :: String
  }
  deriving (Show, Eq, Ord, Generic)

data PackageWithVersionP = PackageWithVersionP
  { _pname :: PackageNameS,
    _pversion :: String
  }
  deriving (Show, Eq, Ord, Generic)

$(makeLenses ''PackageWithVersion)

$(makeLenses ''PackageWithVersionP)

nameVersionP :: Package -> PackageWithVersion
nameVersionP p = PackageWithVersion (nameP p) (prettyShow $ pkgVersion $ package $ _cabal p)

nameVersionF :: PackageFull -> PackageWithVersion
nameVersionF = nameVersionP . _fpackage

---

nameF :: PackageFull -> String
nameF = nameP . _fpackage
