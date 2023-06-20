{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative (Applicative (liftA2))
import Control.Exception (IOException)
import Control.Exception.Base (try)
import Control.Monad (forM_)
import Control.Monad.Writer.Lazy
import Data.Aeson (Value, genericToJSON)
import Data.Aeson.TH (defaultOptions)
import Data.ByteString.UTF8 (fromString)
import Data.Data (Data (gfoldl, toConstr), showConstr)
import Data.Foldable (forM_)
import Distribution.Fields (runParseResult)
import Distribution.ModuleName (ModuleName, toFilePath)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Pretty (prettyShow)
import Distribution.Utils.Path
import qualified GHC.Data.EnumSet as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Errors.Types (DriverMessageOpts (psDiagnosticOpts))
import GHC.Hs (GhcPs, SrcSpanAnn' (..))
import GHC.Parser (parseModule)
import GHC.Parser.Errors.Ppr
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Parser.Lexer (P (unP), PState (..), ParseResult (..), ParserOpts, initParserState, mkParserOpts)
import GHC.Types.Error (DecoratedSDoc, Diagnostic (..))
import GHC.Types.SrcLoc (GenLocated (..), SrcSpan (..), mkRealSrcLoc)
import GHC.Utils.Error (DiagOpts (..), pprMessages)
import GHC.Utils.Outputable (Outputable (..), defaultSDocContext, renderWithContext, SDocContext (..))
import Language.Haskell.Syntax (HsDecl (..), HsModule (..), Sig (..))
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

haskellRunParser :: FilePath -> ParserOpts -> String -> P a -> ParseResult a
haskellRunParser filename opts str parser = unP parser parseState
  where
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = initParserState opts buffer location

showO :: Outputable a => a -> String
showO = renderWithContext defaultSDocContext {sdocErrorSpans = True} . ppr

showS :: Outputable a => SrcSpanAnn' a -> String
showS (SrcSpanAnn ann loc) = showO ann ++ " " ++ showO loc

haskellPath :: FilePath -> CabalLibSrcDir -> ModuleName -> FilePath
haskellPath dir srcDir mod = dir </> getSymbolicPath srcDir </> (toFilePath mod ++ ".hs")

haskellParse :: FilePath -> IO (HsModule GhcPs)
haskellParse p = do
  content <- readFile p
  let diagOpts = DiagOpts EnumSet.empty EnumSet.empty False False Nothing defaultSDocContext
      opts = mkParserOpts EnumSet.empty diagOpts [] False False False False
      r = haskellRunParser p opts content parseModule
  case r of
    POk _ (L l e) -> return e
    PFailed s -> do
      fail (showO $ errors s)

data Definition = Definition
  { definitionIdentifier :: FilePath,
    definitionLocation :: SrcSpan
  }
  deriving (Show, Eq)

haskellGetIdentifiers :: HsModule GhcPs -> WriterT [Definition] IO ()
haskellGetIdentifiers m = do
  forM_ (hsmodDecls m) \d -> do
    let (L a b) = d
    case b of
      SigD e f -> do
        case f of
          TypeSig g h i -> do
            forM_ h \(L (SrcSpanAnn ann loc) k) -> do
              tell [Definition (showO k) loc]
          _ -> return ()
      _ -> return ()

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

        r <- try (haskellParse path) :: IO (Either IOException (HsModule GhcPs))
        case r of
          Left e -> print e
          Right m -> execWriterT (haskellGetIdentifiers m) >>= mapM_ print
