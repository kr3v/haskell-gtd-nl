{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens ((^.))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..), MonadIO (..), runExceptT)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (foldrM)
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
import GTD.Server (modulesOrdered, packagesOrdered, packageOrderedF2)
import GTD.Utils (ultraZoom)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, help, helper, info, long, metavar, option, strOption, (<**>))
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

    c <- case t of
      Package -> do
        mods <- reverse <$> modulesOrdered cPkg
        let modsN = Set.fromList $ HsModule._name <$> mods

        _ <- flip3 foldrM mods Set.empty $ \m acc -> do
          let deps = Set.intersection modsN $ Set.fromList $ module'Dependencies m
          let ds = Set.intersection acc deps

          liftIO $ printf "%s ->\n\t%s\n\t%s\n" (HsModule._name m) (show deps) (show ds)
          return $ Set.insert (HsModule._name m) acc

        return $ digraph (Str $ L.pack "example") $ do
          graphAttrs [RankDir FromLeft]
          flip3 foldrM mods (Set.empty, Set.empty) $ \m (acc, emptyAcc) -> do
            let deps = Set.intersection modsN $ Set.fromList $ module'Dependencies m
            let ds = Set.intersection acc deps

            forM_ ds $ \d -> do
              edge (HsModule._name m) d [Color $ toColorList [Color.X11Color Black]]
            return (Set.insert (HsModule._name m) acc, if Set.null ds then Set.insert (HsModule._name m) emptyAcc else emptyAcc)
      Module -> do
        let nmae = showT2 . Cabal.tuple . Cabal.nameVersionF
        let depsM = fmap (showT2 . Cabal.tuple . Cabal.nameVersionP) . Cabal._dependencies

        pkgs <- reverse <$> packagesOrdered cPkg packageOrderedF2
        let pkgsN = Set.fromList $ nmae <$> pkgs

        liftIO $ print (nmae <$> pkgs)

        _ <- flip3 foldrM pkgs Set.empty $ \m acc -> do
          let deps = Set.intersection pkgsN $ Set.fromList $ depsM m
          let ds = Set.intersection acc deps

          liftIO $ printf "%s ->\n\t%s\n\t%s\n" (nmae m) (show deps) (show ds)
          return $ Set.insert (nmae m) acc

        return $ digraph (Str $ L.pack "example") $ do
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

  case x of
    Left e -> print e
    Right _ -> return ()