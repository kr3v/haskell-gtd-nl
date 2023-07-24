module Main where

import Control.Lens ((^.))
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadIO (..), runExceptT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (foldrM)
import Data.GraphViz (GraphID (Str), GraphvizOutput (Canon), runGraphviz, X11Color (..), RankType (..), rank, GlobalAttributes (attrs))
import Data.GraphViz.Attributes.Complete (Attribute (..), RankDir (FromLeft), toColorList, StyleItem (SItem), StyleName (..))
import Data.GraphViz.Types.Monadic (digraph, edge, graphAttrs, node, cluster)
import qualified Data.Set as Set
import Data.Text.Lazy as L (pack)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (prepareConstants, repos)
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.Module (module'Dependencies)
import GTD.Resolution.State (ccGet, emptyContext)
import GTD.Resolution.State.Caching.Cabal (cabalCacheFetch, cabalFindAtCached)
import GTD.Server (modulesOrdered)
import GTD.Utils (ultraZoom)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, long, strOption, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Data.GraphViz.Attributes.Colors as Color
import Data.GraphViz.Types.Graph (Context(attributes))

data Args = Args
  { _pkgN :: String,
    _pkgV :: String
  }
  deriving (Show)

args :: Parser Args
args = Args <$> strOption (long "packageName" <> help "kekw") <*> strOption (long "packageVersion" <> help "kekw")

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    fullDesc

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f x y z = f z y x

main :: IO ()
main = do
  a@Args {_pkgN = pkgN, _pkgV = pkgV} <- execParser opts
  print a

  init <- getCurrentDirectory
  constants <- prepareConstants
  setCurrentDirectory (constants ^. repos)
  getCurrentDirectory >>= print
  print constants

  runStderrLoggingT $ flip runReaderT constants $ flip evalStateT emptyContext $ do
    cabalCacheFetch
    cPkgM <- ultraZoom ccGet $ runMaybeT $ Cabal.get pkgN pkgV
    case cPkgM of
      Nothing -> liftIO $ print "Cabal.get: no package found"
      Just cPkgP -> do
        cPkgE <- runExceptT $ cabalFindAtCached cPkgP
        case cPkgE of
          Left e -> liftIO $ print e
          Right cPkg -> do
            mods0 <- modulesOrdered cPkg
            let mods = reverse mods0
            let modsN = Set.fromList $ HsModule._name <$> mods

            _ <- flip3 foldrM mods Set.empty $ \m acc -> do
              let deps = Set.intersection modsN $ Set.fromList $ module'Dependencies m
              let ds = Set.intersection acc deps

              liftIO $ printf "%s ->\n\t%s\n\t%s\n" (HsModule._name m) (show deps) (show ds)
              return $ Set.insert (HsModule._name m) acc

            let graphContent = digraph (Str $ L.pack "example") $ do
                  graphAttrs [RankDir FromLeft]
                  flip3 foldrM mods (Set.empty, Set.empty) $ \m (acc, emptyAcc) -> do
                      let deps = Set.intersection modsN $ Set.fromList $ module'Dependencies m
                      let ds = Set.intersection acc deps

                      forM_ ds $ \d -> do
                        edge (HsModule._name m) d [Color $ toColorList [Color.X11Color Black]]
                      return (Set.insert (HsModule._name m) acc, if Set.null ds then Set.insert (HsModule._name m) emptyAcc else emptyAcc)

            _ <- liftIO $ runGraphviz graphContent Canon (init </> "graphviz.actual.gv")
            return ()