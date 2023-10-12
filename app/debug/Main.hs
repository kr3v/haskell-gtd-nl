{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use section" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Exception (try)
import Control.Lens (use, (^.))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalStateT)
import Data.Data (Data (..), showConstr)
import Data.Either (fromRight)
import Data.Foldable (foldrM)
import Data.Generics (listify)
import Data.GraphViz (GraphID (Str), GraphvizOutput (..), X11Color (..), runGraphviz)
import qualified Data.GraphViz.Attributes.Colors as Color
import Data.GraphViz.Attributes.Complete (Attribute (..), RankDir (FromLeft), toColorList)
import Data.GraphViz.Types.Monadic (digraph, edge, graphAttrs)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text.Lazy as L (pack)
import Distribution.Backpack.ComponentsGraph (componentsGraphToList, mkComponentsGraph)
import Distribution.Client.DistDirLayout (defaultDistDirLayout)
import Distribution.Client.HttpUtils (configureTransport)
import Distribution.Client.ProjectConfig (findProjectPackages, findProjectRoot, readProjectConfig)
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.PackageDescription (ComponentName (..), LibraryName (..))
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Compiler (PackageDB (..))
import Distribution.Simple.Flag (Flag (..))
import Distribution.Simple.GHC (configure, getInstalledPackages)
import Distribution.Simple.PackageIndex (allPackages)
import Distribution.Simple.Program (defaultProgramDb)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec (OneComponentRequestedSpec))
import Distribution.Types.CondTree (CondTree (CondNode, condTreeData))
import Distribution.Verbosity (Verbosity)
import qualified GHC.Data.FastString as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config.Parser as GHC
import GHC.Driver.Session as GHC (DynFlags)
import qualified GHC.Driver.Session as GHC
import GHC.Hs as GHC (GhcPs, HsModule, SrcSpanAnnN)
import qualified GHC.Parser as GHC
import GHC.Parser.Lexer as GHC (PState (errors), ParseResult (PFailed, POk))
import qualified GHC.Parser.Lexer as GHC
import GHC.Types.Name.Reader as GHC (RdrName)
import GHC.Types.SourceError as GHC (SourceError)
import GHC.Types.SrcLoc as GHC (GenLocated (..))
import qualified GHC.Types.SrcLoc as GHC
import qualified GTD.Cabal.Cache as Cabal (load)
import GTD.Cabal.FindAt (findAt)
import qualified GTD.Cabal.FindAt as Cabal (findAt)
import GTD.Cabal.Get as Cabal (get)
import qualified GTD.Cabal.Parse as Cabal (__read'packageDescription)
import GTD.Cabal.Types (isMainLib)
import qualified GTD.Cabal.Types as Cabal (Package (..), key, resolve)
import GTD.Configuration (defaultArgs, prepareConstants, repos)
import GTD.Haskell.Module (emptyHsModule, emptyMetadata)
import qualified GTD.Haskell.Module as HsModule
import GTD.Haskell.Parser.GhcLibParser (fakeSettings, parsePragmasIntoDynFlags, showO)
import qualified GTD.Resolution.Cache.Resolution as ResolutionCache
import GTD.Resolution.Module (orderedByDependencies)
import GTD.Resolution.Module.Single (module'Dependencies)
import GTD.Resolution.Package (package'dependencies'ordered, package'order'default)
import GTD.Server.Definition (cabalPackage, findAtF)
import qualified GTD.Server.Definition as Server (resolution)
import GTD.State (ccGet, emptyContext)
import GTD.Utils (fromMaybeM, maybeM)
import Options.Applicative (Parser, ParserInfo, auto, command, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, strOption, subparser, (<**>))
import System.Directory (getCurrentDirectory, makeAbsolute, setCurrentDirectory)
import System.FilePath (isRelative, (</>))
import System.IO (BufferMode (LineBuffering), hPrint, hSetBuffering, stderr, stdout)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BSC8

showT2 :: (String, String) -> String
showT2 (a, b) = "(" ++ a ++ "," ++ b ++ ")"

data CabalPackage = CabalPackage
  { _name :: String,
    _version :: String
  }
  deriving (Show)

cp :: Parser CabalPackage
cp =
  CabalPackage
    <$> strOption (long "packageName" <> help "package name; Cabal's main library will be used")
    <*> strOption (long "packageVersion" <> help "package version in Cabal predicate form (e.g. `^>= 0.1`)")

data OrderType = Package | Module deriving (Show, Read, Enum, Bounded)

data Args
  = Order {_pkg :: CabalPackage, _type :: OrderType}
  | Identifier {_text :: String}
  | ParseHeader {_file :: String}
  | Cabal {_fileC :: String, _fileP :: String, _root :: String}
  | Resolution {_rpkg :: ResolutionPackage, _identifier :: String}
  | Playground {_file :: FilePath}
  deriving (Show)

data ResolutionPackage
  = PCabal {_cpkg :: CabalPackage, _module :: String}
  | PDir {_dir :: FilePath, _dfile :: FilePath}
  deriving (Show)

ro :: Parser Args
ro = Order <$> cp <*> option auto (long "type" <> metavar "ENUM" <> help "either Package (for Cabal dependencies resolution order) or Module (for Haskell modules resolution order)")

idP :: Parser Args
idP = Identifier <$> strOption (long "text" <> help "identifier to be parsed")

ph :: Parser Args
ph = ParseHeader <$> strOption (long "file" <> help "path to a file to be parsed")

ca :: Parser Args
ca =
  Cabal
    <$> strOption (long "fileC" <> help "path to `*.cabal` file")
    <*> strOption (long "fileP" <> help "path to `cabal.project` file")
    <*> strOption (long "root" <> help "path to project root")

resP :: Parser ResolutionPackage
resP = do
  let c = PCabal <$> cp <*> strOption (long "module" <> help "kekw")
      d = PDir <$> strOption (long "dir" <> help "kekw") <*> strOption (long "file" <> help "kekw")
      commands =
        [ command "cabal" (info c (fullDesc <> progDesc "kekw")),
          command "dir" (info d (fullDesc <> progDesc "kekw"))
        ]
  subparser (mconcat commands)

res :: Parser Args
res = Resolution <$> resP <*> strOption (long "identifier" <> help "kekw")

args :: Parser Args
args = do
  let commands =
        [ command "order" (info ro (fullDesc <> progDesc "gtd-nl-hs: print processing order for either Cabal dependencies or Haskell modules of a given package")),
          command "identifier" (info idP (fullDesc <> progDesc "ghc-lib-parser: try parsing given identifier")),
          command "header" (info ph (fullDesc <> progDesc "ghc-lib-parser: try parsing a file's header")),
          command "cabal" (info ca (fullDesc <> progDesc "cabal: just testing API, nothing useful")),
          command "resolution" (info res (fullDesc <> progDesc "gtd-nl-hs: check if a given identifier is resolvable in a given package and module")),
          command "playground" (info (Playground <$> strOption (long "file" <> help "kekw")) (fullDesc <> progDesc "ghc-lib-parser: try parsing a file"))
        ]
  subparser (mconcat commands)

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    fullDesc

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f x y z = f z y x

order :: String -> String -> OrderType -> IO ()
order pkgN pkgV t = do
  wd <- getCurrentDirectory
  constants <- prepareConstants =<< defaultArgs
  setCurrentDirectory (constants ^. repos)
  getCurrentDirectory >>= print
  print constants

  x <- runStderrLoggingT $ flip runReaderT constants $ flip evalStateT emptyContext $ runExceptT $ do
    Cabal.load
    ccGetM <- use ccGet
    cPkgM <- fst <$> Cabal.get ccGetM pkgN pkgV
    cPkgP <- case cPkgM of
      Nothing -> throwError "Cabal.get: no package found"
      Just cPkgP -> return cPkgP

    let h nmae depsM f g = do
          cPkg <- head <$> g cPkgP
          pkgs <- reverse <$> f cPkg
          let pkgsN = Set.fromList $ nmae <$> pkgs

          liftIO $ print (nmae <$> pkgs)

          (_, (h1, h2)) <- flip3 foldrM pkgs (Set.empty, (0, 0)) $ \m (acc1, (acc21, acc22)) -> do
            let deps = Set.intersection pkgsN $ Set.fromList $ depsM m
            let ds = Set.intersection acc1 deps

            liftIO $ printf "%s ->\n\t%s\n\t%s\n" (show $ nmae m) (show deps) (show ds)
            return (Set.insert (nmae m) acc1, (acc21 + Set.size deps, acc22 + Set.size ds))

          liftIO $ printf "h1: %d, h2: %d\n" h1 h2

          let c = digraph (Str $ L.pack "example") $ do
                graphAttrs [RankDir FromLeft]
                flip3 foldrM pkgs (Set.empty, Set.empty) $ \m (acc, emptyAcc) -> do
                  let deps = Set.intersection pkgsN $ Set.fromList $ depsM m
                  let ds = Set.intersection acc deps

                  forM_ ds $ \d -> do
                    edge (nmae m) d [Color $ toColorList [Color.X11Color Black]]
                  return (Set.insert (nmae m) acc, if Set.null ds then Set.insert (nmae m) emptyAcc else emptyAcc)
          _ <- liftIO $ runGraphviz c Canon (wd </> "graphviz.gv")
          _ <- liftIO $ runGraphviz c Svg (wd </> "graphviz.svg")
          return ()

    case t of
      Module -> h (BSC8.unpack . HsModule._name) (fmap show . module'Dependencies) orderedByDependencies findAtF
      Package -> h (show . Cabal.key) (fmap show . Cabal._dependencies) (flip package'dependencies'ordered package'order'default) Cabal.findAt

  case x of
    Left e -> print e
    Right _ -> return ()

identifier :: String -> IO ()
identifier txt = do
  let content = GHC.stringToStringBuffer $ "{-# LANGUAGE OverloadedRecordDot #-}\nmodule Main where\n" ++ txt

  let dynFlags0 = GHC.defaultDynFlags fakeSettings
  fM <- try (parsePragmasIntoDynFlags dynFlags0 "." content) :: IO (Either SourceError (Maybe (DynFlags, [String])))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  print languagePragmas

  let o = GHC.initParserOpts dynFlags
      l = GHC.mkRealSrcLoc (GHC.mkFastString ".") 1 1
      b = GHC.stringToStringBuffer $ txt
      s = GHC.initParserState o b l
      r = GHC.unP GHC.parseIdentifier s

  print $ case r of
    POk _ (L _ e) -> printf ":t %s => %s" (showConstr . toConstr $ e) (showO e)
    PFailed e -> showO $ errors e

parseHeader :: String -> IO ()
parseHeader file = do
  content <- GHC.stringToStringBuffer <$> readFile file

  let dynFlags0 = GHC.defaultDynFlags fakeSettings
  fM <- try (parsePragmasIntoDynFlags dynFlags0 "." content) :: IO (Either SourceError (Maybe (DynFlags, [String])))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  print languagePragmas

  let o = GHC.initParserOpts dynFlags
      l = GHC.mkRealSrcLoc (GHC.mkFastString file) 1 1
      b = content
      s = GHC.initParserState o b l
      r = GHC.unP GHC.parseHeader s

  print $ case r of
    POk _ (L _ e) -> printf ":t %s => %s" (showConstr . toConstr $ e) (showO e)
    PFailed e -> showO $ errors e

cabal :: String -> String -> String -> IO ()
cabal fileP fileC root = do
  let Right (v :: Verbosity) = eitherParsec "normal"
  Right r <- findProjectRoot (Just root) Nothing
  let ddl = defaultDistDirLayout r Nothing
  http <- configureTransport v [] (Just "curl")

  CondNode {condTreeData = pc} <- runRebuild root $ readProjectConfig v http NoFlag NoFlag ddl
  print pc
  locs <- runRebuild root $ findProjectPackages ddl pc
  print locs

  let cs = OneComponentRequestedSpec $ CLibName LMainLibName
  pd <- runStdoutLoggingT $ Cabal.__read'packageDescription fileC
  case mkComponentsGraph cs pd of
    Left e -> print e
    Right r -> print $ componentsGraphToList r

  (c, _, d) <- configure v Nothing Nothing defaultProgramDb
  idx <- getInstalledPackages v c [GlobalPackageDB, UserPackageDB] d
  forM_ (allPackages idx) print

-- cabal v2-run haskell-gtd-nl-debug -- resolution dir --dir "$(pwd)" --file "./app/server/Main.hs" --identifier "return"
-- cabal v2-run haskell-gtd-nl-debug -- resolution cabal --packageName base --packageVersion "^>=4.16" --module "Prelude" --identifier "return" 2>/dev/null
resolution :: ResolutionPackage -> String -> IO ()
resolution pkg identS = do
  let ident = BSC8.pack identS
  constants <- prepareConstants =<< defaultArgs
  setCurrentDirectory (constants ^. repos)

  r <- runStderrLoggingT $ flip runReaderT constants $ flip evalStateT emptyContext $ runExceptT $ do
    Cabal.load
    (cPkg, f) <- case pkg of
      PCabal {_cpkg = CabalPackage {..}, _module = mS} -> do
        let m = BSC8.pack mS
        ccGetM <- use ccGet
        pkgP <- fst <$> Cabal.get ccGetM _name _version
        cPkgs <- maybeM (throwError "`cabal get` could not find a package by given name and version") findAt (return pkgP)
        cPkg <- fromMaybeM (throwError "the package was found, but there's no 'main library' in it") $ return $ find isMainLib cPkgs
        f <- fromMaybeM (throwError "the package was found, but the utility cannot find an appropriate `*.hs` file for given module name in any source directory") $ liftIO $ Cabal.resolve cPkg m
        fA <- liftIO $ makeAbsolute f
        return (cPkg, fA)
      PDir {_dir = dir, _dfile = f} -> do
        fA <- liftIO $ makeAbsolute $ if isRelative f then dir </> f else f
        liftIO $ hPrint stderr fA
        (,fA) . head <$> cabalPackage dir fA

    liftIO $ hPrint stderr $ Cabal.key cPkg
    liftIO $ hPrint stderr f
    let m = emptyHsModule {HsModule._metadata = emptyMetadata {HsModule._mPath = f, HsModule._mPkgK = Cabal.key cPkg}}
    r <- fromMaybeM (throwError "for a given module in a given package, there's no 'resolution' cache") $ ResolutionCache.get m
    w <- Server.resolution r ident
    liftIO $ print w

  case r of
    Left e -> print e
    Right _ -> return ()

playground :: FilePath -> IO ()
playground file = do
  content <- GHC.stringToStringBuffer <$> readFile file

  let dynFlags0 = GHC.defaultDynFlags fakeSettings
  fM <- try (parsePragmasIntoDynFlags dynFlags0 "." content) :: IO (Either SourceError (Maybe (DynFlags, [String])))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  print languagePragmas

  let o = GHC.initParserOpts dynFlags
      l = GHC.mkRealSrcLoc (GHC.mkFastString file) 1 1
      b = content
      s = GHC.initParserState o b l
      r = GHC.unP GHC.parseModule s

  case r of
    PFailed e -> print $ showO $ errors e
    POk _ (L _ e) -> do
      let collectIds :: GHC.HsModule GhcPs -> [GenLocated SrcSpanAnnN RdrName]
          collectIds = listify isRdrName

          isRdrName :: GenLocated l RdrName -> Bool
          isRdrName _ = True

      printf ":t %s\n" (showConstr . toConstr $ e)
      let identifiers = collectIds e
      forM_ identifiers $ \(L l i) -> do
        printf "l=%s, i=%s\n" (showO l) (showO i)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  a <- execParser opts
  hPrint stderr a

  case a of
    Order {_pkg = CabalPackage {..}, _type = t} -> order _name _version t
    Identifier {_text = text} -> identifier text
    ParseHeader {_file = file} -> parseHeader file
    Cabal {_fileP = fileP, _fileC = fileC, _root = root} -> cabal fileP fileC root
    Resolution {_rpkg = pkg, _identifier = ident} -> resolution pkg ident
    Playground {_file = file} -> playground file
