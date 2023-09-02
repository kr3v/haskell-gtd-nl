{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Cabal.Dependencies where

import Control.Applicative (Applicative (..))
import Control.Concurrent.Async.Lifted (forConcurrently)
import Control.Lens (At (..), use, (%=), (.=))
import Control.Monad (forM)
import Control.Monad.Logger (MonadLoggerIO (..))
import Control.Monad.RWS (MonadReader (..), MonadState, asks)
import Control.Monad.State (StateT (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing)
import Distribution.Pretty (prettyShow)
import Distribution.Types.VersionRange (withinRange)
import GTD.Cabal.Get (get)
import GTD.Cabal.Parse (parse)
import GTD.Cabal.Types (Dependency (..), Designation (..), DesignationType (..), Package (..), PackageWithResolvedDependencies, PackageWithUnresolvedDependencies)
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context (..), cLocalPackages, ccFull, ccGet)
import GTD.Utils (deduplicate, logDebugNSS, mapFrom)
import System.FilePath ((</>))
import Text.Printf (printf)

__full ::
  PackageWithUnresolvedDependencies ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m PackageWithResolvedDependencies
__full pkg = do
  let logTag = printf "cabal full %s" $ show $ Cabal.key pkg

  let deps = deduplicate $ _dependencies pkg
  logDebugNSS logTag $ "all dependencies: " ++ show deps

  locallyResolvedDepsM <- forM deps $ \Dependency {..} -> do
    localsM <- use $ cLocalPackages . at (_dName, _dSubname)
    let locals = maybe [] (fmap snd . Map.toList) localsM
    let matching = find ((`withinRange` _dVersion) . _version) locals
    return matching
  let locallyResolvedDeps = catMaybes locallyResolvedDepsM
  logDebugNSS logTag $ printf "locally resolved dependencies: %s" $ show $ Cabal.key <$> locallyResolvedDeps

  let unresolvedDeps = Map.elems $ Map.difference (mapFrom _dName deps) (mapFrom _name locallyResolvedDeps)

  -- execute `cabal get` on all dependencies concurrently
  st <- use ccGet
  let dep d@Dependency {..} = (,) d <$> get _dName (prettyShow _dVersion)
  (nameToPathMs, caches) <- unzip <$> forConcurrently unresolvedDeps (flip runStateT st . runMaybeT . dep)
  ccGet .= foldr (<>) st caches
  let pkgs = catMaybes nameToPathMs

  reposR <- asks _repos
  -- parse resolved dependencies' cabal files
  depsR <- forM pkgs $ \(Dependency {..}, p) -> do
    r <- parse $ reposR </> p </> (_dName ++ ".cabal")
    -- we are only interested in the package main library
    return $ find (liftA2 (&&) (isNothing . _desName) ((== Library) . _desType) . _designation) r
  logDebugNSS logTag $ printf "resolved dependencies: %s" $ show $ Cabal.key <$> catMaybes depsR

  return $ pkg {_dependencies = locallyResolvedDeps ++ catMaybes depsR}

full ::
  Cabal.PackageWithUnresolvedDependencies ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m Cabal.PackageWithResolvedDependencies
full pkg = do
  let k = Cabal.key pkg
  e <- use $ ccFull . at k
  case e of
    Just d -> return d
    Nothing -> do
      r <- __full pkg
      ccFull %= Map.insert k r
      return r
