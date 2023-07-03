{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.Enrich where

import qualified Data.Map as Map
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.AST (ClassOrData (..))
import qualified GTD.Haskell.AST as Declarations
import GTD.Haskell.Declaration (Declaration (..))
import GTD.Haskell.Module (HsModuleP (..))
import qualified GTD.Haskell.Module as HsModule

enrichTryPackage0 ::
  (Map.Map ModuleNameS HsModuleP -> a -> Maybe a) ->
  a ->
  Map.Map ModuleNameS HsModuleP ->
  Maybe a
enrichTryPackage0 f d ms = f ms d

enrichTryPackage ::
  Declaration ->
  Map.Map ModuleNameS HsModuleP ->
  Maybe Declaration
enrichTryPackage = enrichTryPackage0 enrichTryModule

enrichTryPackageCDT ::
  ClassOrData ->
  Map.Map ModuleNameS HsModuleP ->
  Maybe ClassOrData
enrichTryPackageCDT = enrichTryPackage0 enrichTryModuleCDT

enrichTryModule ::
  Map.Map ModuleNameS HsModuleP ->
  Declaration ->
  Maybe Declaration
enrichTryModule moduleDecls orig = do
  m <- _declModule orig `Map.lookup` moduleDecls
  mDecl <- _declName orig `Map.lookup` (Declarations._decls . HsModule._exports) m
  return $ orig {_declSrcOrig = _declSrcOrig mDecl}

enrichTryModuleCDT ::
  Map.Map ModuleNameS HsModuleP ->
  ClassOrData ->
  Maybe ClassOrData
enrichTryModuleCDT moduleDecls orig = do
  m <- (_declModule . _cdtName) orig `Map.lookup` moduleDecls
  (_declName . _cdtName) orig `Map.lookup` (Declarations._dataTypes . HsModule._exports) m
  -- flip execState cdt $ do
  --   cdtName . declSrcUsage .= view (cdtName . declSrcUsage) orig
  --   cdtFields . traverse %= (\d -> d {_declSrcUsage = _declSrcUsage $ fromMaybe d $ view (cdtFields . at (_declName d)) orig})