{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Cabal.Full where

import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Lens (At (..), use, (%=))
import Control.Monad (forM)
import Control.Monad.Logger (MonadLoggerIO (..))
import Control.Monad.RWS (MonadReader (..), MonadState, asks)
import Control.Monad.State (MonadState (put), StateT (..))
import qualified Control.Monad.State.Lazy as State
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import GTD.Cabal.Get (GetCache, get)
import GTD.Cabal.Package (Dependency (Dependency, _dName, _dVersion), Designation (_desName, _desType), DesignationType (Library), Package (_dependencies, _designation, _name), PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, dAsT)
import qualified GTD.Cabal.Package as Cabal
import GTD.Cabal.Parse (parse)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context (..), ccFull, ccGet)
import GTD.Utils (deduplicate, logDebugNSS, ultraZoom)
import System.FilePath ((</>))

__full :: PackageWithUnresolvedDependencies -> (MonadBaseControl IO m, MonadLoggerIO m, MonadState GetCache m, MonadReader GTDConfiguration m) => m PackageWithResolvedDependencies
__full pkg = do
  logDebugNSS "cabal fetch" $ _name pkg
  let deps = _dependencies pkg
  let namesWithPredicates = deduplicate $ (\(Dependency {_dName = n, _dVersion = v}) -> (n, v)) <$> deps
  logDebugNSS "cabal fetch" $ "dependencies: " ++ show namesWithPredicates
  st <- State.get
  pkgs <-
    let fetchDependency n vp = (,,) n vp <$> get n vp
        fetchDependencies ds = do
          (nameToPathMs, caches) <- unzip <$> forConcurrently ds (flip runStateT st . runMaybeT . uncurry fetchDependency)
          put $ foldr (<>) st caches
          return $ catMaybes nameToPathMs
     in fetchDependencies namesWithPredicates
  reposR <- asks _repos
  depsF <- fmap (Map.fromList . concat) $ forM pkgs $ \(n, v, p) -> do
    r <- parse $ reposR </> p </> (n ++ ".cabal")
    let libs = filter ((== Library) . _desType . _designation) r
    return $ fmap (\l -> (,) (n, v, _desName . _designation $ l) l) libs
  let depsR = Map.restrictKeys depsF (Set.fromList $ dAsT <$> deps)
  return $ pkg {_dependencies = Map.elems depsR}

full ::
  Cabal.PackageWithUnresolvedDependencies ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m Cabal.PackageWithResolvedDependencies
full pkg = do
  let k = Cabal.key pkg
  e <- use $ ccFull . at k
  case e of
    Just d -> return d
    Nothing -> do
      r <- ultraZoom ccGet $ __full pkg
      ccFull %= Map.insert k r
      return r
