{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GTD.Cabal where

import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Lens (At (..), makeLenses, set, use, view, (%=), (%~), (.=))
import Control.Monad (forM, forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLogger, MonadLoggerIO (..))
import Control.Monad.RWS (MonadReader (..), MonadState (put), asks)
import Control.Monad.State (StateT (..))
import qualified Control.Monad.State.Lazy as State
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.UTF8 as BS
import Data.List (find)
import qualified Data.Map as Map
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
import GTD.Utils (deduplicate, logDebugNSS, logDebugNSS', logErrorNSS)
import System.Directory (listDirectory)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO (IOMode (ReadMode), hGetContents, openFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

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

data GetCache = GetCache
  { _vs :: Map.Map String (Maybe FilePath),
    _changed :: Bool
  }
  deriving (Show, Generic)

$(makeLenses ''GetCache)

instance FromJSON GetCache

instance ToJSON GetCache

instance Semigroup GetCache where
  (<>) :: GetCache -> GetCache -> GetCache
  (GetCache vs1 c1) <> (GetCache vs2 c2) = GetCache (vs1 <> vs2) (c1 || c2)

instance Monoid GetCache where
  mempty :: GetCache
  mempty = GetCache mempty False


-- executes `cabal get` on given `pkg + pkgVerPredicate`
get :: String -> String -> (MonadIO m, MonadLogger m, MonadState GetCache m, MonadReader GTDConfiguration m) => MaybeT m FilePath
get pkg pkgVerPredicate = do
  let k = pkg ++ pkgVerPredicate
  r0 <- use $ vs . at k
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
      logDebugNSS' "cabal get" $ printf "cabal get %s %s: exit code %s" pkg pkgVerPredicate (show ec)

      let r = find (not . null) packageVersion
      vs %= Map.insert k r
      changed .= True

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

__dependencies :: Package -> (MonadBaseControl IO m, MonadLoggerIO m, MonadState GetCache m, MonadReader GTDConfiguration m) => m [Package]
__dependencies pkg = do
  let c = view cabal pkg
  logDebugNSS "cabal fetch" $ prettyShow $ packageName c
  let deps = concatMap targetBuildDepends $ allBuildInfo c
  let names = deduplicate $ (\(Dependency n v _) -> (unPackageName n, prettyShow v)) <$> deps
  logDebugNSS "cabal fetch" $ "dependencies: " ++ show names
  st <- State.get
  pkgs <-
    let fetchDependency n vp = (,) n <$> get n vp
        fetchDependencies ds = do
          (c, d) <- unzip <$> forConcurrently ds (flip runStateT st . runMaybeT . uncurry fetchDependency)
          put $ foldr (<>) st d
          return $ catMaybes c
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

full :: Package -> (MonadBaseControl IO m, MonadLoggerIO m, MonadState GetCache m, MonadReader GTDConfiguration m) => m PackageFull
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
