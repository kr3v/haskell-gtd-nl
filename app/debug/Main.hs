{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..), MonadIO (..), runExceptT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Data (Data (..), showConstr)
import Data.Foldable (foldrM)
import Data.GraphViz (GraphID (Str), GraphvizOutput (..), X11Color (..), runGraphviz)
import qualified Data.GraphViz.Attributes.Colors as Color
import Data.GraphViz.Attributes.Complete (Attribute (..), RankDir (FromLeft), toColorList)
import Data.GraphViz.Types.Monadic (digraph, edge, graphAttrs)
import qualified Data.Set as Set
import Data.Text.Lazy as L (pack)
import qualified GHC.Data.FastString as GHC
import qualified GHC.Data.StringBuffer as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Session as GHC
import qualified GHC.Parser as GHC
import GHC.Parser.Lexer
import qualified GHC.Parser.Lexer as GHC
import GHC.Types.SrcLoc (GenLocated (..))
import qualified GHC.Types.SrcLoc as GHC
import qualified GTD.Cabal as Cabal
import GTD.Configuration (prepareConstants, repos)
import qualified GTD.Haskell.Module as HsModule
import GTD.Haskell.Parser.GhcLibParser (fakeSettings, showO, parsePragmasIntoDynFlags)
import GTD.Resolution.Module (module'Dependencies)
import GTD.Resolution.State (ccGet, emptyContext)
import GTD.Resolution.State.Caching.Cabal (cabalCacheFetch, cabalFindAtCached)
import GTD.Server (modulesOrdered, packageOrderedF2, packagesOrdered)
import GTD.Utils (ultraZoom)
import Options.Applicative (Parser, ParserInfo, auto, command, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, strOption, subparser, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import GHC.Types.SourceError (SourceError)
import GHC.Driver.Session
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Control.Exception (try)
import qualified Language.Haskell.Exts as GHC
import GHC.Parser.PostProcess

showT2 :: (String, String) -> String
showT2 (a, b) = "(" ++ a ++ "," ++ b ++ ")"

data Type = Package | Module deriving (Show, Read, Enum, Bounded)

data Args
  = ResolutionOrder {_pkgN :: String, _pkgV :: String, _type :: Type}
  | Identifier {_text :: String}
  deriving (Show)

ro :: Parser Args
ro =
  ResolutionOrder
    <$> strOption (long "packageName" <> help "kekw")
    <*> strOption (long "packageVersion" <> help "kekw")
    <*> option auto (long "type" <> metavar "ENUM" <> help "kekw")

idP :: Parser Args
idP = Identifier <$> strOption (long "text" <> help "kekw")

args :: Parser Args
args = do
  let commands =
        [ command "order" (info ro (fullDesc <> progDesc "kekw")),
          command "identifier" (info idP (fullDesc <> progDesc "kekw"))
        ]
  subparser (mconcat commands)

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    fullDesc

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f x y z = f z y x

main :: IO ()
main = do
  a <- execParser opts
  print a

  case a of
    ResolutionOrder {_pkgN = pkgN, _pkgV = pkgV, _type = t} -> do
      init <- getCurrentDirectory
      constants <- prepareConstants
      setCurrentDirectory (constants ^. repos)
      getCurrentDirectory >>= print
      print constants

      x :: Either String () <- runStderrLoggingT $ runExceptT $ flip runReaderT constants $ flip evalStateT emptyContext $ do
        cabalCacheFetch
        cPkgM <- ultraZoom ccGet $ runMaybeT $ Cabal.get pkgN pkgV
        cPkgP <- case cPkgM of
          Nothing -> throwError "Cabal.get: no package found"
          Just cPkgP -> return cPkgP
        cPkg <- cabalFindAtCached cPkgP

        let h nmae depsM f = do
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
              _ <- liftIO $ runGraphviz c Canon (init </> "graphviz.gv")
              _ <- liftIO $ runGraphviz c Svg (init </> "graphviz.svg")
              return ()

        case t of
          Module -> h HsModule._name module'Dependencies modulesOrdered
          Package -> h (showT2 . Cabal.tuple . Cabal.nameVersionF) (fmap (showT2 . Cabal.tuple . Cabal.nameVersionP) . Cabal._dependencies) (flip packagesOrdered packageOrderedF2)

      case x of
        Left e -> print e
        Right _ -> return ()
    Identifier {_text = text} -> do
      let content = "{-# LANGUAGE OverloadedRecordDot #-}\nmodule Main where\n" ++ text

      let dynFlags0 = GHC.defaultDynFlags fakeSettings
      fM <- try (parsePragmasIntoDynFlags dynFlags0 "." content) :: IO (Either SourceError (Maybe (DynFlags, [String])))
      let (dynFlags, languagePragmas) = fromMaybe (dynFlags0, []) $ fromRight Nothing fM

      print languagePragmas

      let opts = GHC.initParserOpts dynFlags
          location = GHC.mkRealSrcLoc (GHC.mkFastString ".") 1 1
          buffer = GHC.stringToStringBuffer text
          parseState = GHC.initParserState opts buffer location
          r = GHC.unP GHC.parseIdentifier parseState

      print $ case r of
        POk _ e -> printf ":t %s => %s" (showConstr . toConstr $ e) (showO e)
        PFailed s -> showO $ errors s