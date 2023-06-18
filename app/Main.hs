{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (IOException)
import Control.Exception.Base (try)
import Control.Monad (forM_)
import Data.ByteString.UTF8 (fromString)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription (BuildInfo (..), Dependency (Dependency), Executable (buildInfo, exeName), Library (..), PackageDescription (..), unPackageName)
import qualified Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (majorBoundVersion, mkVersion)
import qualified Distribution.Types.BuildInfo as BuildInfo
import Distribution.Utils.Path (PackageDir, SourceDir, SymbolicPath, getSymbolicPath)
import Language.Haskell.Exts (Decl (..), Module (..), ParseMode (..), ParseResult (..), defaultParseMode, parseFile, parseFileContents, parseFileWithCommentsAndPragmas, prettyPrint, parseFileContentsWithMode)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hClose, hGetBuf, hGetContents, openFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

import GTD.Cabal
import GTD.Haskell

main :: IO ()
main = do
  pkg <- cabalRead "haskell-gtd.cabal"
  deps <- cabalFetchDependencies pkg
  forM_ deps $ \(n, d) -> do
    let path = d </> (n ++ ".cabal")
    print path

    pkg <- cabalRead path
    deps <- cabalFetchDependencies pkg
    (srcDirs, modules) <- cabalGetExportedModules pkg
    print modules
    print deps

    forM_ modules $ \mod -> do
      forM_ srcDirs $ \srcDir -> do
        let path = haskellPath d srcDir mod
        print path

        ec <- (try (readFile path) :: IO (Either IOException String))
        case ec of
          Left e -> print e
          Right content -> do
            postCpp <- haskellApplyCppHs path content
            case haskellParse path postCpp of
              Left e -> print e
              Right m -> do
                haskellGetIdentifiers m
                haskellGetExportedIdentifiers m
