{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell where

import Control.Lens (At (..), makeLenses, use, view, (%=))
import Control.Monad (forM_, guard, unless)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadState, execStateT, get, modify)
import Control.Monad.Writer (WriterT (..), execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (CabalPackage (..), ModuleNameS, PackageNameS)
import GTD.Haskell.AST (Exports (..), Imports (..), haskellGetExports, haskellGetImports)
import GTD.Haskell.Declaration (Declaration (..), hasNonEmptyOrig)
import GTD.Haskell.Enrich (enrichTryModule, enrichTryPackage)
import GTD.Haskell.Module (HsModule (..))
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Haskell.Walk (MS (MS), parseChosenModules)
import qualified GTD.Haskell.Walk as MS
import GTD.Utils (flipTuple, logDebugNSS, logErrorNSS)
import Text.Printf (printf)

data ContextCabalPackage = ContextCabalPackage
  { _ccpmodules :: Map.Map PackageNameS (Map.Map ModuleNameS HsModule),
    _dependencies :: [CabalPackage],
    _cabalCache :: Map.Map PackageNameS CabalPackage
  }
  deriving (Show, Generic)

$(makeLenses ''ContextCabalPackage)

parsePackages :: (MonadLoggerIO m, MonadState ContextCabalPackage m) => m ()
parsePackages = do
  let logTag = "parse packages"
  deps <- use dependencies
  st <- use ccpmodules
  logDebugNSS logTag $ printf "already parsed packages: %s" (show $ Map.keys st)
  forM_ deps $ \dep -> do
    unless (Map.member (_cabalPackageName dep) st) $ do
      logDebugNSS logTag $ printf "parsing %s..." (show $ _cabalPackageName dep)
      mods <- parsePackage dep
      ccpmodules %= Map.insert (_cabalPackageName dep) mods

parsePackage0 ::
  CabalPackage ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModule)
parsePackage0 p = do
  let logTag = "parse package 0"
  let ms = MS Map.empty Set.empty
  ms' <- execStateT (parseChosenModules p (Map.keys $ _cabalPackageExportedModules p)) ms
  return $ MS._modules ms'

parsePackage ::
  CabalPackage ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModule)
parsePackage p = do
  let logTag = "parse package 1"
  modules <- parsePackage0 p

  let iModules :: Map.Map Int HsModule = Map.fromList $ zip [1 ..] (Map.elems modules)
  let iNModules = Map.fromList $ flipTuple <$> Map.assocs (_name <$> iModules)

  let jF :: Int -> ModuleNameS -> (Int, Int)
      jF i dep = (i, fromJust $ Map.lookup dep iNModules)
      iF :: Int -> HsModule -> [(Int, Int)]
      iF i mod = jF i <$> filter (`Map.member` iNModules) (_deps mod)
  let graph = Graph.buildG (1, Map.size modules) $ concatMap (uncurry iF) (Map.assocs iModules)
  let graphS = Graph.reverseTopSort graph

  let oModules = (\i -> fromJust $ Map.lookup i iModules) <$> graphS

  execStateT (forM_ oModules moduleEvalExports) Map.empty

---

getExportedModule ::
  ContextCabalPackage ->
  ModuleNameS ->
  Either String HsModule
getExportedModule ContextCabalPackage {_ccpmodules = mods, _dependencies = deps} modN = do
  let modules = flip mapMaybe deps $ \pkg -> do
        guard $ modN `Map.member` _cabalPackageExportedModules pkg
        dependencyModules <- _cabalPackageName pkg `Map.lookup` mods
        modN `Map.lookup` dependencyModules
  case length modules of
    0 -> Left $ printf "no package seem to export" (show modN)
    1 -> Right $ head modules
    _ -> Left $ printf "multiple matches for %s: %s" (show modN) (show $ _name <$> modules)

getAllImports :: HsModule -> (MonadLoggerIO m, MonadState ContextCabalPackage m) => m [Declaration]
getAllImports mod = do
  let logTag = "get all imports for " <> show (_name mod)
  Imports importsS importedModules <- execWriterT $ haskellGetImports (_ast mod)
  ctx <- get
  let (errorsM, importsM) = partitionEithers $ getExportedModule ctx <$> importedModules
  forM_ errorsM $ \err -> logErrorNSS logTag err
  return $ importsS ++ concatMap (Map.elems . _exports) importsM

---

enrich0 :: Declaration -> (MonadLoggerIO m, MonadState ContextCabalPackage m) => m Declaration
enrich0 d = do
  let logTag = "enrich"
  deps <- use dependencies
  mods <- use ccpmodules
  let xs = filter hasNonEmptyOrig $ mapMaybe (enrichTryPackage d mods) deps
  case length xs of
    0 -> return d
    1 -> return $ head xs
    _ -> do
      logDebugNSS logTag $ printf "multiple matches for %s: %s" (show d) (show xs)
      return $ head xs

enrich :: HsModule -> (MonadLoggerIO m, MonadState ContextCabalPackage m) => m HsModule
enrich mod = do
  let logTag = "module enrich for " ++ _name mod
  importsE <- getAllImports mod >>= mapM enrich0
  return $ mod {_decls = _decls mod <> asDeclsMap importsE}

---

moduleEvalExports :: HsModule -> (MonadLoggerIO m, MonadState (Map.Map ModuleNameS HsModule) m) => m ()
moduleEvalExports mod = do
  let logTag = "module prepare exports for " ++ _name mod
  logDebugNSS logTag $ _name mod
  let locals = _decls mod

  st <- get
  (isImplicitExportAll, Exports exportsS exportsM) <- runWriterT $ haskellGetExports (_ast mod)
  let exports = exportsS ++ concatMap (Map.elems . _exports) (mapMaybe (\n -> view (at n) st) exportsM)

  Imports importsS importsM <- execWriterT $ haskellGetImports (_ast mod)
  let imports = importsS ++ concatMap (Map.elems . _exports) (mapMaybe (\n -> view (at n) st) importsM)
  let importsE = fmap (\d -> fromMaybe d (enrichTryModule st d)) imports

  -- drop `isImplicitExportAll then locals` cases from `topSort` invocation to avoid circular dependencies (cycles)
  let m = mod {_exports = if isImplicitExportAll then locals else Map.intersection (locals <> asDeclsMap importsE) (asDeclsMap exports)}
  modify $ Map.insert (_name mod) m
