{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Use section" #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Exception (try)
import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..), MonadIO (..), runExceptT)
import Control.Monad.Logger (runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Data (Data (..), showConstr)
import Data.Either (fromRight)
import Data.Foldable (foldrM)
import Data.GraphViz (GraphID (Str), GraphvizOutput (..), X11Color (..), runGraphviz)
import qualified Data.GraphViz.Attributes.Colors as Color
import Data.GraphViz.Attributes.Complete (Attribute (..), RankDir (FromLeft), toColorList)
import Data.GraphViz.Types.Monadic (digraph, edge, graphAttrs)
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
import GHC.Driver.Session (DynFlags)
import qualified GHC.Driver.Session as GHC
import qualified GHC.Parser as GHC
import GHC.Parser.Lexer (PState (errors), ParseResult (PFailed, POk))
import qualified GHC.Parser.Lexer as GHC
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc (GenLocated (..))
import qualified GHC.Types.SrcLoc as GHC
import qualified GTD.Cabal.Cache as Cabal (load)
import qualified GTD.Cabal.FindAt as Cabal (findAt)
import qualified GTD.Cabal.Get as Cabal (getS)
import qualified GTD.Cabal.Types as Cabal (Package (_dependencies), key)
import qualified GTD.Cabal.Parse as Cabal (__read'packageDescription)
import GTD.Configuration (defaultArgs, prepareConstants, repos)
import qualified GTD.Haskell.Module as HsModule
import GTD.Haskell.Parser.GhcLibParser (fakeSettings, parsePragmasIntoDynFlags, showO)
import GTD.Resolution.Module (module'Dependencies)
import GTD.Resolution.State (ccGet, emptyContext)
import GTD.Server (modulesOrdered, package'dependencies'ordered, package'order'default, findAtF)
import GTD.Utils (ultraZoom)
import Options.Applicative (Parser, ParserInfo, auto, command, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, strOption, subparser, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Text.Printf (printf)

showT2 :: (String, String) -> String
showT2 (a, b) = "(" ++ a ++ "," ++ b ++ ")"

data Type = Package | Module deriving (Show, Read, Enum, Bounded)

data Args
  = ResolutionOrder {_pkgN :: String, _pkgV :: String, _type :: Type}
  | Identifier {_text :: String}
  | ParseHeader {_file :: String}
  | Cabal {_fileC :: String, _fileP :: String, _root :: String}
  deriving (Show)

ro :: Parser Args
ro =
  ResolutionOrder
    <$> strOption (long "packageName" <> help "kekw")
    <*> strOption (long "packageVersion" <> help "kekw")
    <*> option auto (long "type" <> metavar "ENUM" <> help "kekw")

idP :: Parser Args
idP = Identifier <$> strOption (long "text" <> help "kekw")

ph :: Parser Args
ph = ParseHeader <$> strOption (long "file" <> help "kekw")

cp :: Parser Args
cp =
  Cabal
    <$> strOption (long "fileC" <> help "kekw")
    <*> strOption (long "fileP" <> help "kekw")
    <*> strOption (long "root" <> help "kekw")

args :: Parser Args
args = do
  let commands =
        [ command "order" (info ro (fullDesc <> progDesc "kekw")),
          command "identifier" (info idP (fullDesc <> progDesc "kekw")),
          command "header" (info ph (fullDesc <> progDesc "kekw")),
          command "cabal" (info cp (fullDesc <> progDesc "kekw"))
        ]
  subparser (mconcat commands)

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    fullDesc

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f x y z = f z y x

resolutionOrder :: String -> String -> Type -> IO ()
resolutionOrder pkgN pkgV t = do
  wd <- getCurrentDirectory
  constants <- prepareConstants =<< defaultArgs
  setCurrentDirectory (constants ^. repos)
  getCurrentDirectory >>= print
  print constants

  x <- runStderrLoggingT $ runExceptT $ flip runReaderT constants $ flip evalStateT emptyContext $ do
    Cabal.load
    cPkgM <- ultraZoom ccGet $ Cabal.getS pkgN pkgV
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

            liftIO $ printf "%s ->\n\t%s\n\t%s\n" (nmae m) (show deps) (show ds)
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
      Module -> h HsModule._name module'Dependencies modulesOrdered findAtF
      Package -> h (show . Cabal.key) (fmap show . Cabal._dependencies) (flip package'dependencies'ordered package'order'default) Cabal.findAt

  case x of
    Left e -> print e
    Right _ -> return ()

identifier :: String -> IO ()
identifier txt = do
  let content = "{-# LANGUAGE OverloadedRecordDot #-}\nmodule Main where\n" ++ txt

  let dynFlags0 = GHC.defaultDynFlags fakeSettings
  fM <- try (parsePragmasIntoDynFlags dynFlags0 "." content) :: IO (Either SourceError (Maybe (DynFlags, [String])))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  print languagePragmas

  let o = GHC.initParserOpts dynFlags
      l = GHC.mkRealSrcLoc (GHC.mkFastString ".") 1 1
      b = GHC.stringToStringBuffer txt
      s = GHC.initParserState o b l
      r = GHC.unP GHC.parseIdentifier s

  print $ case r of
    POk _ (L _ e) -> printf ":t %s => %s" (showConstr . toConstr $ e) (showO e)
    PFailed e -> showO $ errors e

parseHeader :: String -> IO ()
parseHeader file = do
  content <- readFile file

  let dynFlags0 = GHC.defaultDynFlags fakeSettings
  fM <- try (parsePragmasIntoDynFlags dynFlags0 "." content) :: IO (Either SourceError (Maybe (DynFlags, [String])))
  let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

  print languagePragmas

  let o = GHC.initParserOpts dynFlags
      l = GHC.mkRealSrcLoc (GHC.mkFastString file) 1 1
      b = GHC.stringToStringBuffer content
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  a <- execParser opts
  print a

  case a of
    ResolutionOrder {_pkgN = pkgN, _pkgV = pkgV, _type = t} -> resolutionOrder pkgN pkgV t
    Identifier {_text = text} -> identifier text
    ParseHeader {_file = file} -> parseHeader file
    Cabal {_fileP = fileP, _fileC = fileC, _root = root} -> cabal fileP fileC root
