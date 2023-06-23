{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Cabal where

import Control.Applicative (Applicative (liftA2))
import Control.Lens (makeLenses, view)
import Control.Monad (forM)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Trans (MonadIO (liftIO), lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.UTF8 as BS
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Distribution.Compat.Prelude (Generic)
import Distribution.ModuleName (ModuleName, fromString, toFilePath)
import Distribution.PackageDescription (BuildInfo (..), Dependency (Dependency), Executable (buildInfo, exeName), Library (..), PackageDescription (..), unPackageName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (majorBoundVersion, mkVersion)
import Distribution.Utils.Path (PackageDir, SourceDir, SymbolicPath, getSymbolicPath)
import Distribution.Utils.ShortText (fromShortText)
import GTD.Configuration (GTDConfiguration (..), repos)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hClose, hGetBuf, hGetContents, openFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

cabalGet :: String -> String -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => MaybeT m String
cabalGet pkg pkgVerPredicate = do
  reposR <- view repos
  (_, Just hout, Just herr, h) <- liftIO $ createProcess (proc "cabal" ["get", pkg ++ pkgVerPredicate, "--destdir", reposR]) {std_out = CreatePipe, std_err = CreatePipe}
  stdout <- liftIO $ hGetContents hout
  stderr <- liftIO $ hGetContents herr
  let content = stdout ++ stderr
  let re = pkg ++ "-" ++ "[^\\/]*\\/"
  let packageVersion :: [String] = (=~ re) <$> lines content
  liftIO $ waitForProcess h
  MaybeT $ return $ find (not . null) packageVersion

cabalRead :: FilePath -> (MonadLoggerIO m) => m PackageDescription
cabalRead p = do
  handle <- liftIO $ openFile p ReadMode
  (warnings, epkg) <- liftIO $ runParseResult . parseGenericPackageDescription . BS.fromString <$> hGetContents handle
  liftIO $ either (fail . show) (return . flattenPackageDescription) epkg

cabalFetchDependencies :: PackageDescription -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m [(String, FilePath)]
cabalFetchDependencies pkg = do
  let dependencies = liftA2 (,) exeName (((\(Dependency n v _) -> (unPackageName n, prettyShow v)) <$>) . targetBuildDepends . buildInfo) <$> executables pkg
      fetchDependency n v = (,) n <$> cabalGet n v
      fetchDependencies :: [(String, String)] -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m [(String, FilePath)]
      fetchDependencies deps = catMaybes <$> mapM runMaybeT (uncurry fetchDependency <$> deps)
  concat <$> mapM (\(component, deps) -> fetchDependencies deps) dependencies

type CabalLibSrcDir = SymbolicPath PackageDir SourceDir

cabalGetExportedModules :: PackageDescription -> (MonadLoggerIO m) => m ([SymbolicPath PackageDir SourceDir], [ModuleName])
cabalGetExportedModules pkg = do
  lib <- liftIO $ maybe (fail "no library") return (library pkg)
  let modules = exposedModules lib
  let srcDirs = (hsSourceDirs . libBuildInfo) lib
  return (srcDirs, modules)

haskellPath :: FilePath -> CabalLibSrcDir -> ModuleNameS -> FilePath
haskellPath dir src mod = dir </> getSymbolicPath src </> ((toFilePath . fromString $ mod) ++ ".hs")

type PackageNameS = String

type ModuleNameS = String

data CabalPackage = CabalPackage
  { _cabalPackageName :: PackageNameS,
    _cabalPackagePath :: FilePath,
    _cabalPackageDesc :: PackageDescription,
    _cabalPackageSrcDirs :: [CabalLibSrcDir],
    _cabalPackageExportedModules :: Map.Map ModuleNameS ModuleName
  }
  deriving (Show, Generic)

$(makeLenses ''CabalPackage)

cabalDeps :: PackageDescription -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m [CabalPackage]
cabalDeps pkg = do
  reposR <- view repos
  deps <- cabalFetchDependencies pkg
  forM deps $ \(n, p') -> do
    let p = reposR </> p'
    let path = p </> (n ++ ".cabal")
    desc <- cabalRead path
    (srcs, mods) <- cabalGetExportedModules desc
    return $ CabalPackage n p desc srcs (Map.fromList $ (\x -> (prettyShow x, x)) <$> mods)
