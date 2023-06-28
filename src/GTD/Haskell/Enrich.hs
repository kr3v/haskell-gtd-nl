{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.Enrich where

import Control.Lens (At (..), set, view, (%=), (.=))
import Control.Monad.RWS (guard)
import Control.Monad.State (execState)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import GTD.Cabal (CabalPackage (_cabalPackageExportedModules, _cabalPackageName), ModuleNameS, PackageNameS)
import GTD.Haskell.AST (ClassOrData (..), cdtFields, cdtName)
import qualified GTD.Haskell.AST as Declarations
import GTD.Haskell.Declaration (Declaration (..), declSrcUsage)
import GTD.Haskell.Module (HsModuleP (_exports))

enrichTryPackage0 ::
  (a -> ModuleNameS) ->
  (Map.Map ModuleNameS HsModuleP -> a -> Maybe a) ->
  a ->
  Map.Map PackageNameS (Map.Map ModuleNameS HsModuleP) ->
  CabalPackage ->
  Maybe a
enrichTryPackage0 n f d ms dep = do
  guard $ n d `Map.member` _cabalPackageExportedModules dep
  dependencyModules <- _cabalPackageName dep `Map.lookup` ms
  f dependencyModules d

enrichTryPackage ::
  Declaration ->
  Map.Map PackageNameS (Map.Map ModuleNameS HsModuleP) ->
  CabalPackage ->
  Maybe Declaration
enrichTryPackage = enrichTryPackage0 _declModule enrichTryModule

enrichTryPackageCDT ::
  ClassOrData ->
  Map.Map PackageNameS (Map.Map ModuleNameS HsModuleP) ->
  CabalPackage ->
  Maybe ClassOrData
enrichTryPackageCDT = enrichTryPackage0 (_declModule . _cdtName) enrichTryModuleCDT

enrichTryModule ::
  Map.Map ModuleNameS HsModuleP ->
  Declaration ->
  Maybe Declaration
enrichTryModule moduleDecls orig = do
  m <- _declModule orig `Map.lookup` moduleDecls
  mDecl <- _declName orig `Map.lookup` (Declarations._decls . _exports) m
  return $ orig {_declSrcOrig = _declSrcOrig mDecl}

enrichTryModuleCDT ::
  Map.Map ModuleNameS HsModuleP ->
  ClassOrData ->
  Maybe ClassOrData
enrichTryModuleCDT moduleDecls orig = do
  m <- (_declModule . _cdtName) orig `Map.lookup` moduleDecls
  cdt <- (_declName . _cdtName) orig `Map.lookup` (Declarations._dataTypes . _exports) m
  return $ flip execState cdt $ do
    cdtName . declSrcUsage .= view (cdtName . declSrcUsage) orig
    cdtFields . traverse %= (\d -> d {_declSrcUsage = _declSrcUsage $ fromMaybe d $ view (cdtFields . at (_declName d)) orig})