{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Main (main) where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (IOException)
import Control.Exception.Base (try)
import Control.Monad (forM_)
import Data.Aeson (Value, genericToJSON)
import Data.Aeson.TH (defaultOptions)
import Data.ByteString.UTF8 (fromString)
import Data.Data (Data (gfoldl, toConstr), showConstr)
import Data.Foldable (forM_)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription (BuildInfo (..), Dependency (Dependency), Executable (buildInfo, exeName), Library (..), PackageDescription (..), unPackageName)
import qualified Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Distribution.Simple (majorBoundVersion, mkVersion)
import qualified Distribution.Types.BuildInfo as BuildInfo
import Distribution.Utils.Path (PackageDir, SourceDir, SymbolicPath, getSymbolicPath)
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (SrcSpanAnn' (..))
import GHC.Parser (parseModule)
import GHC.Parser.Lexer (P (unP), ParseResult (..), ParserOpts, initParserState, mkParserOpts)
import GHC.Types.SrcLoc (GenLocated (..), mkRealSrcLoc)
import GHC.Utils.Error (DiagOpts (..))
import GHC.Utils.Outputable (Outputable (..), defaultSDocContext, renderWithContext)
import Language.Haskell.Exts (Decl (..), Module (..), ParseMode (..), ParseResult (..), defaultParseMode, parseFile, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Haskell.Syntax
import Language.Haskell.Syntax.Binds
import Language.Haskell.Syntax.Extension (XRec (..))
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (IOMode (ReadMode), hClose, hGetBuf, hGetContents, openFile)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

cabalGet :: String -> String -> IO String
cabalGet pkg pkgVerPredicate = do
  (_, Just hout, Just herr, _) <- createProcess (proc "cabal" ["get", pkg ++ pkgVerPredicate]) {std_out = CreatePipe, std_err = CreatePipe}
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
haskellPath dir srcDir mod = dir </> getSymbolicPath srcDir </> (toFilePath mod ++ ".hs")

haskellParse :: FilePath -> IO (Either String (Module SrcSpanInfo))
haskellParse p = do
  r <- parseFileWithCommentsAndPragmas defaultParseMode {baseLanguage = Haskell2010} p
  case r of
    ParseOk (m, _, _) -> return $ Right m
    ParseFailed loc e -> return $ Left $ printf "failed to parse %s: %s @ %s" p e (show loc)

haskellGetIdentifiers :: Module SrcSpanInfo -> IO ()
haskellGetIdentifiers p = do
  let (Module src head wtf1 imports wtf2) = p
  forM_ wtf2 $ \decl ->
    case decl of
      TypeSig src ident _ -> print ident
      _ -> return ()
  print "<<<<***>>>>\n"

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

        r <- try (haskellParse path) :: IO (Either IOException (Either String (Module SrcSpanInfo)))
        case r of
          Left e -> print e
          Right (Left e) -> print e
          Right (Right m) -> haskellGetIdentifiers m
