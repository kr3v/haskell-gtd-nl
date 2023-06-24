{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.Enrich where

import Control.Monad.RWS (guard)
import qualified Data.Map as Map
import GTD.Cabal (CabalPackage (_cabalPackageExportedModules, _cabalPackageName), ModuleNameS, PackageNameS)
import GTD.Haskell.Declaration (Declaration (..), Identifier (..))
import GTD.Haskell.Module (HsModule (_exports))

enrichTryPackage ::
  Declaration ->
  Map.Map PackageNameS (Map.Map ModuleNameS HsModule) ->
  CabalPackage ->
  Maybe Declaration
enrichTryPackage d mods dep = do
  guard $ _declModule d `Map.member` _cabalPackageExportedModules dep
  dependencyModules <- _cabalPackageName dep `Map.lookup` mods
  enrichTryModule dependencyModules d

enrichTryModule ::
  Map.Map ModuleNameS HsModule ->
  Declaration ->
  Maybe Declaration
enrichTryModule moduleDecls orig = do
  mod <- _declModule orig `Map.lookup` moduleDecls
  mDecl <- Identifier (_declName orig) `Map.lookup` _exports mod
  return $ orig {_declSrcOrig = _declSrcOrig mDecl}

