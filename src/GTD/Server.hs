{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server where

import Control.Lens (over, use, (%=))
import Control.Monad (forM_, mapAndUnzipM, when, (<=<), (>=>))
import Control.Monad.Except (ExceptT, MonadError, MonadIO (..), liftEither, runExceptT)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..))
import Control.Monad.State (evalStateT, execStateT, modify)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Cache.LRU as LRU
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Stats (getRTSStats, getRTSStatsEnabled)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Haskell.Declaration (Declaration (..), SourceSpan, hasNonEmptyOrig)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.Definition (enrich, resolution)
import GTD.Resolution.Module (figureOutExports, figureOutExports0, module'Dependencies, moduleR)
import GTD.Resolution.State (Context (..), Package (Package, _cabalPackage, _modules), cExports, ccGet)
import qualified GTD.Resolution.State as Package
import GTD.Resolution.State.Caching.Cabal (cabalCacheStore, cabalFindAtCached, cabalFull)
import GTD.Resolution.State.Caching.Package (packageCachedAdaptSizeTo, packageCachedGet, packageCachedGet', packageCachedPut, packagePersistenceGet, persistenceExists)
import GTD.Resolution.Utils (ParallelizedState (..), SchemeState (..), parallelized, scheme)
import GTD.Utils (logDebugNSS, ultraZoom)
import Text.Printf (printf)

data DefinitionRequest = DefinitionRequest
  { workDir :: FilePath,
    file :: FilePath,
    word :: String
  }
  deriving (Show, Generic)

data DefinitionResponse = DefinitionResponse
  { srcSpan :: Maybe SourceSpan,
    err :: Maybe String
  }
  deriving (Show, Generic, Eq)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

noDefintionFoundError :: Monad m => ExceptT String m a
noDefintionFoundError = throwE "No definition found"

noDefintionFoundErrorME :: MonadError String m => m a
noDefintionFoundErrorME = liftEither $ noDefintionFoundErrorE "No definition found"

noDefintionFoundErrorE :: (Monad m) => m (Either String a)
noDefintionFoundErrorE = runExceptT noDefintionFoundError

---

modules :: Package -> (MonadBaseControl IO m, MonadLoggerIO m) => m Package
modules pkg@Package {_cabalPackage = c} = do
  mods <- modules1 pkg c
  return pkg {Package._exports = Map.restrictKeys mods (Cabal._exports . Cabal._modules $ c), Package._modules = mods}

modules1 ::
  Package ->
  Cabal.PackageFull ->
  (MonadBaseControl IO m, MonadLoggerIO m) => m (Map.Map ModuleNameS HsModuleP)
modules1 pkg c = do
  modsO <- flip runReaderT c $ flip evalStateT (SchemeState Map.empty Map.empty) $ do
    scheme moduleR HsModule._name id (return . module'Dependencies) (Set.toList . Cabal._exports . Cabal._modules $ c)
  -- execStateT (forM_ modsO figureOutExports) (_modules pkg)

  let st = ParallelizedState modsO Map.empty Map.empty (_modules pkg)
  parallelized st (Cabal.nameVersionF c) figureOutExports0 (const "tbd") HsModule._name (return . module'Dependencies)

---

packageK ::
  Context ->
  Cabal.PackageFull ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Package, Context -> Context)
packageK c cPkg = do
  (pkgM, f) <- packageCachedGet' c cPkg
  case pkgM of
    Just x -> return (Just x, f)
    Nothing -> do
      (depsC, m) <- bimap catMaybes (foldr (.) id) <$> mapAndUnzipM (packageCachedGet' c <=< (flip evalStateT c . cabalFull)) (Cabal._dependencies cPkg)
      let deps = foldr (<>) Map.empty $ Package._exports <$> depsC
      pkg <- modules $ Package {_cabalPackage = cPkg, Package._modules = deps, Package._exports = Map.empty}
      packageCachedPut cPkg pkg
      logDebugNSS "packageK" $ printf "packageK: I do want to insert %s into `cExports`" (show $ Cabal.nameVersionF cPkg)
      return (Just pkg, over cExports (LRU.insert (Cabal.nameVersionF cPkg) (Package._exports pkg)) . m . f)

package0 ::
  Cabal.PackageFull ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m (Maybe Package)
package0 cPkg = do
  c <- get
  (a, b) <- packageK c cPkg
  modify b
  return a

simpleShowContext :: Context -> String
simpleShowContext c =
  printf
    "ccFindAt: %s, ccFull: %s, ccGet: %s, cExports: %s\nccFindAt = %s\nccFull = %s\nccGet = %s\ncExports = %s"
    (show $ Map.size $ _ccFindAt c)
    (show $ Map.size $ _ccFull c)
    (show $ Map.size $ Cabal._vs . _ccGet $ c)
    (show $ LRU.size $ _cExports c)
    (show $ Map.keys $ _ccFindAt c)
    (show $ Map.keys $ _ccFull c)
    (show $ Map.keys $ Cabal._vs . _ccGet $ c)
    (show $ fst <$> LRU.toList (_cExports c))

package ::
  Cabal.PackageFull ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m (Maybe Package)
package cPkg0 = do
  m <- packagePersistenceGet cPkg0
  case m of
    Just p -> return $ Just p
    Nothing -> do
      pkgsO <- flip evalStateT (SchemeState Map.empty Map.empty) $ do
        scheme (\cPkg -> do b <- persistenceExists cPkg; if b then return Nothing else ((Just <$>) . cabalFull) cPkg) Cabal.nameVersionF Cabal.nameVersionP (return . Cabal._dependencies) (Cabal._dependencies cPkg0)

      -- forM_ pkgsO package0

      stC0 <- get
      let st = ParallelizedState pkgsO Map.empty Map.empty stC0
      stC1 <- parallelized st ("packages", Cabal.nameVersionF cPkg0) packageK simpleShowContext Cabal.nameVersionF (return . fmap Cabal.nameVersionP . Cabal._dependencies)
      put stC1

      package0 cPkg0
      packagePersistenceGet cPkg0

---

definition ::
  DefinitionRequest ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf, word = w}) = do
  cPkg <- cabalFindAtCached wd
  pkgM <- package cPkg
  pkg <- maybe noDefintionFoundErrorME return pkgM

  m <- parseModule emptyHsModule {_path = rf}
  m' <- resolution <$> evalStateT (enrich m) pkg

  ccGC <- use $ ccGet . Cabal.changed
  when ccGC $ do
    cabalCacheStore

  statsE <- liftIO getRTSStatsEnabled
  when statsE $ do
    stats <- liftIO getRTSStats
    liftIO $ putStrLn $ "GCs: " ++ show stats

  ultraZoom cExports $ packageCachedAdaptSizeTo (toInteger $ 10 + length (Cabal._dependencies cPkg))

  case w `Map.lookup` m' of
    Nothing -> noDefintionFoundErrorME
    Just d ->
      if hasNonEmptyOrig d
        then return $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
        else noDefintionFoundErrorME
