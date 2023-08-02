{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (forM, forM_)
import Control.Monad.Except (MonadError (..), MonadIO (..), runExceptT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (foldrM)
import qualified Data.Graph as Graph
import Data.GraphViz (GraphID (Str), GraphvizOutput (..), X11Color (..), runGraphviz)
import qualified Data.GraphViz.Attributes.Colors as Color
import Data.GraphViz.Attributes.Complete (Attribute (..), RankDir (FromLeft), toColorList)
import Data.GraphViz.Types.Monadic (digraph, edge, graphAttrs)
import qualified Data.Set as Set
import Data.Text.Lazy as L (pack)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (prepareConstants, repos)
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.Module (module'Dependencies)
import GTD.Resolution.State (ccGet, emptyContext)
import GTD.Resolution.State.Caching.Cabal (cabalCacheFetch, cabalFindAtCached)
import GTD.Server (modulesOrdered, packageOrderedF2, packagesOrdered)
import GTD.Utils (ultraZoom)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, help, helper, info, liftA3, long, metavar, option, strOption, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import Text.Printf (printf)

showT2 :: (String, String) -> String
showT2 (a, b) = "(" ++ a ++ "," ++ b ++ ")"

data Type = Package | Module deriving (Show, Read, Enum, Bounded)

data Args = Args
  { _pkgN :: String,
    _pkgV :: String,
    _type :: Type
  }
  deriving (Show)

args :: Parser Args
args =
  Args
    <$> strOption (long "packageName" <> help "kekw")
    <*> strOption (long "packageVersion" <> help "kekw")
    <*> option auto (long "type" <> metavar "ENUM" <> help "kekw")

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    fullDesc

flip3 :: (a -> b -> c -> d) -> (c -> b -> a -> d)
flip3 f x y z = f z y x

main :: IO ()
main = do
  a@Args {_pkgN = pkgN, _pkgV = pkgV, _type = t} <- execParser opts
  print a

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

    let l = [(id, "id"), (reverse, "reverse")]
    forM_ (liftA3 (,,) l l l) $ \((of1, on1), (of2, on2), (of3, on3)) -> do
      let h nmae depsM f = do
            pkgs <- reverse <$> f cPkg
            let pkgsN = Set.fromList $ nmae <$> pkgs

            liftIO $ print (nmae <$> pkgs)

            (_, (h1, h2)) <- flip3 foldrM pkgs (Set.empty, (0, 0)) $ \m (acc1, (acc21, acc22)) -> do
              let deps = Set.intersection pkgsN $ Set.fromList $ depsM m
              let ds = Set.intersection acc1 deps

              liftIO $ printf "%s ->\n\t%s\n\t%s\n" (nmae m) (show deps) (show ds)
              return (Set.insert (nmae m) acc1, (acc21 + Set.size deps, acc22 + Set.size ds))

            liftIO $ printf "o=%s => h1: %d, h2: %d\n" (show (on1, on2, on3)) h1 h2

            let c = digraph (Str $ L.pack "example") $ do
                  graphAttrs [RankDir FromLeft]
                  flip3 foldrM pkgs (Set.empty, Set.empty) $ \m (acc, emptyAcc) -> do
                    let deps = Set.intersection pkgsN $ Set.fromList $ depsM m
                    let ds = Set.intersection acc deps

                    forM_ ds $ \d -> do
                      edge (nmae m) d [Color $ toColorList [Color.X11Color Black]]
                    return (Set.insert (nmae m) acc, if Set.null ds then Set.insert (nmae m) emptyAcc else emptyAcc)
            -- _ <- liftIO $ runGraphviz c Canon (init </> "graphviz.gv")
            -- _ <- liftIO $ runGraphviz c Svg (init </> "graphviz.svg")
            return ()

      case t of
        Module -> h HsModule._name module'Dependencies (modulesOrdered (of1, of2, of3))
        Package -> h (showT2 . Cabal.tuple . Cabal.nameVersionF) (fmap (showT2 . Cabal.tuple . Cabal.nameVersionP) . Cabal._dependencies) (\p -> packagesOrdered (of1, of2, of3) p packageOrderedF2)

  case x of
    Left e -> print e
    Right _ -> return ()