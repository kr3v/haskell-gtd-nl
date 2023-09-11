{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Cabal.Dependencies where

import Control.Applicative (Applicative (..))
import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Lens (use, (%~), (.=))
import Control.Monad (forM)
import Control.Monad.Logger (MonadLoggerIO (..))
import Control.Monad.RWS (MonadReader (..), MonadState, asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing)
import Distribution.Pretty (prettyShow)
import Distribution.Types.VersionRange (withinRange)
import GTD.Cabal.Get (get)
import GTD.Cabal.Parse (parse)
import GTD.Cabal.Types (Dependency (..), Designation (..), DesignationType (..), Package (..), PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, isMainLib)
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context (..), ccFull, ccGet)
import GTD.Utils (deduplicate, logDebugNSS, mapFrom)
import System.FilePath ((</>))
import Text.Printf (printf)

__full ::
  Context ->
  PackageWithUnresolvedDependencies ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m) => m (PackageWithResolvedDependencies, Context -> Context)
__full ctx pkg = do
  let logTag = printf "cabal full %s" $ show $ Cabal.key pkg

  let deps = deduplicate $ _dependencies pkg
  logDebugNSS logTag $ "all dependencies: " ++ show deps

  locallyResolvedDepsM <- forM deps $ \Dependency {..} -> do
    let localsM = (_dName, _dSubname) `Map.lookup` _cLocalPackages ctx
    let locals = maybe [] (fmap snd . Map.toList) localsM
    let matching = find ((`withinRange` _dVersion) . _version) locals
    return matching
  let locallyResolvedDeps = catMaybes locallyResolvedDepsM
  logDebugNSS logTag $ printf "locally resolved dependencies: %s" $ show $ Cabal.key <$> locallyResolvedDeps

  let unresolvedDeps = Map.elems $ Map.difference (mapFrom _dName deps) (mapFrom _name locallyResolvedDeps)

  -- execute `cabal get` on all dependencies concurrently

  let st = _ccGet ctx
      dep d@Dependency {..} = do
        (v, m) <- get st _dName (prettyShow _dVersion)
        return ((d,) <$> v, m)

  (nameToPathMs, cacheM) <- unzip <$> forConcurrently unresolvedDeps dep
  let pkgs = catMaybes nameToPathMs

  reposR <- asks _repos
  -- parse resolved dependencies' cabal files
  depsR <- forM pkgs $ \(Dependency {..}, p) -> do
    r <- parse $ reposR </> p </> (_dName ++ ".cabal")
    -- we are only interested in the package main library
    return $ find isMainLib r
  logDebugNSS logTag $ printf "resolved dependencies: %s" $ show $ Cabal.key <$> catMaybes depsR

  return (pkg {_dependencies = locallyResolvedDeps ++ catMaybes depsR}, ccGet %~ flip (foldr ($)) cacheM)

fullS ::
  PackageWithUnresolvedDependencies ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m PackageWithResolvedDependencies
fullS pkg = do
  ctx <- use id
  (r, m) <- full ctx pkg
  id .= m ctx
  return r

full ::
  Context ->
  Cabal.PackageWithUnresolvedDependencies ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Cabal.PackageWithResolvedDependencies, Context -> Context)
full ctx pkg = do
  let k = Cabal.key pkg
  case k `Map.lookup` _ccFull ctx of
    Just d -> return (d, id)
    Nothing -> do
      (r, m) <- __full ctx pkg
      return (r, (ccFull %~ Map.insert k r) . m)
