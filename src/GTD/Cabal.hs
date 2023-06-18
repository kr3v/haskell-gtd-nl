{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Cabal where

import Control.Applicative (Applicative (liftA2))
import Data.ByteString.UTF8 (fromString)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription (BuildInfo (..), Dependency (Dependency), Executable (buildInfo, exeName), Library (..), PackageDescription (..), unPackageName)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (majorBoundVersion, mkVersion)
import Distribution.Utils.Path (PackageDir, SourceDir, SymbolicPath, getSymbolicPath)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hClose, hGetBuf, hGetContents, openFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

cabalGet :: String -> String -> IO String
cabalGet pkg pkgVerPredicate = do
  (_, Just hout, Just herr, _) <- createProcess (proc "cabal" ["get", pkg ++ pkgVerPredicate, "--destdir", "./repo"]) {std_out = CreatePipe, std_err = CreatePipe}
  stdout <- hGetContents hout
  stderr <- hGetContents herr
  let content = stdout ++ stderr
  let re = pkg ++ "-" ++ "[^\\/]*\\/"
  let packageVersion :: [String] = (=~ re) <$> lines content
  return $ head $ filter (not . null) packageVersion

cabalRead :: FilePath -> IO PackageDescription
cabalRead p = do
  handle <- openFile p ReadMode
  (warnings, epkg) <- runParseResult . parseGenericPackageDescription . fromString <$> hGetContents handle
  either (fail . show) (return . flattenPackageDescription) epkg

cabalFetchDependencies :: PackageDescription -> IO [(String, FilePath)]
cabalFetchDependencies pkg = do
  let dependencies = liftA2 (,) exeName (((\(Dependency n v _) -> (unPackageName n, prettyShow v)) <$>) . targetBuildDepends . buildInfo) <$> executables pkg
      fetchDependency n v = (,) n <$> cabalGet n v
      fetchDependencies = mapM (uncurry fetchDependency)
  concat <$> mapM (\(component, deps) -> fetchDependencies deps) dependencies

type CabalLibSrcDir = SymbolicPath PackageDir SourceDir

cabalGetExportedModules :: PackageDescription -> IO ([SymbolicPath PackageDir SourceDir], [ModuleName])
cabalGetExportedModules pkg = do
  lib <- maybe (fail "no library") return (library pkg)
  let modules = exposedModules lib
  let srcDirs = (hsSourceDirs . libBuildInfo) lib
  return (srcDirs, modules)

haskellPath :: FilePath -> CabalLibSrcDir -> ModuleName -> FilePath
haskellPath dir src mod = dir </> getSymbolicPath src </> (toFilePath mod ++ ".hs")
