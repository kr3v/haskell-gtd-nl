{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server where

import Control.Lens (use)
import Control.Monad (forM_, when, (>=>))
import Control.Monad.Except (ExceptT, MonadError, MonadIO (..), liftEither, runExceptT)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..))
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GHC.Stats (getRTSStats)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Haskell.Declaration (Declaration (..), SourceSpan, hasNonEmptyOrig)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.Definition (enrich, resolution)
import GTD.Resolution.Module (figureOutExports, module'Dependencies, moduleR)
import GTD.Resolution.State (Context, Package (Package, _cabalPackage, _modules), ccGet, cExports)
import qualified GTD.Resolution.State as Package
import GTD.Resolution.State.Caching.Cabal (cabalCacheStore, cabalFindAtCached, cabalFull)
import GTD.Resolution.State.Caching.Package (packageCachedGet, packageCachedPut, packagePersistenceGet, persistenceExists, packageCachedAdaptSizeTo)
import GTD.Resolution.Utils (SchemeState (..), scheme)
import GTD.Utils (ultraZoom)

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

modules :: Package -> (MonadLoggerIO m) => m Package
modules pkg@Package {_cabalPackage = c} = do
  mods <- modules1 pkg c
  return pkg {Package._exports = Map.restrictKeys mods (Cabal._exports . Cabal._modules $ c), Package._modules = mods}

modules1 ::
  Package ->
  Cabal.PackageFull ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModuleP)
modules1 pkg c = do
  modsO <- flip runReaderT c $ flip evalStateT (SchemeState Map.empty Map.empty) $ do
    scheme moduleR HsModule._name id module'Dependencies (Set.toList . Cabal._exports . Cabal._modules $ c)
  execStateT (forM_ modsO figureOutExports) (_modules pkg)

---

package0 ::
  Cabal.PackageFull ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m ()
package0 cPkg = do
  pkgM <- packageCachedGet cPkg
  case pkgM of
    Just _ -> return ()
    Nothing -> do
      depsC <- catMaybes <$> mapM (cabalFull >=> packageCachedGet) (Cabal._dependencies cPkg)
      let deps = foldr (<>) Map.empty $ Package._exports <$> depsC
      pkg <- modules $ Package {_cabalPackage = cPkg, Package._modules = deps, Package._exports = Map.empty}
      packageCachedPut cPkg pkg

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
      forM_ pkgsO package0
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

  stats <- liftIO getRTSStats
  liftIO $ putStrLn $ "GCs: " ++ show stats

  ultraZoom cExports $ packageCachedAdaptSizeTo (toInteger $ 10 + length (Cabal._dependencies cPkg))

  case w `Map.lookup` m' of
    Nothing -> noDefintionFoundErrorME
    Just d ->
      if hasNonEmptyOrig d
        then return $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
        else noDefintionFoundErrorME
 