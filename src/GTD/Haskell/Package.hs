{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Package where

import Control.Lens (makeLenses, use, (%=), (.=), (^.))
import Control.Monad.Cont (forM, forM_, unless)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State.Lazy (MonadState (..), execStateT, modify)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import Control.Monad.Trans.Writer (WriterT (..), execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (CabalLibSrcDir, CabalPackage (..), ModuleNameS, PackageNameS, cabalPackageName, haskellPath)
import GTD.Haskell.AST (ClassOrData (..), Declarations (..), Exports (..), Imports (..), allImportedModules, haskellGetExports, haskellGetIdentifiers, haskellGetImports)
import GTD.Haskell.Declaration (Declaration (..))
import GTD.Haskell.Enrich (enrichTryModule, enrichTryModuleCDT)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (flipTuple, logDebugNSS, logErrorNSS, mapFrom)
import Text.Printf (printf)

---

type With a b = (a, b)

type HsModuleWithDependencies = With HsModule [ModuleNameS]

module'Dependencies :: HsModule -> (MonadLoggerIO m) => m [ModuleNameS]
module'Dependencies m = do
  eM <- execWriterT $ haskellGetExports (_ast m)
  iM <- execWriterT $ haskellGetImports (_ast m)
  return $ exportedModules eM ++ allImportedModules iM

module'2 ::
  CabalPackage ->
  ModuleNameS ->
  (MonadLoggerIO m) => m [Either (CabalLibSrcDir, ModuleNameS, String) HsModule]
module'2 p m = do
  let root = _cabalPackagePath p
  let srcDirs = _cabalPackageSrcDirs p
  forM srcDirs $ \srcDir -> runExceptT $ do
    let path = haskellPath root srcDir m
    let cm = emptyHsModule {_package = p ^. cabalPackageName, _name = m, _path = path}
    withExceptT (srcDir,m,) (parseModule cm)

module'1 ::
  CabalPackage ->
  ModuleNameS ->
  (MonadLoggerIO m) => m ([String], Maybe HsModule)
module'1 p mod = do
  es <- module'2 p mod

  let (errors, modules) = partitionEithers es
  let errorsS = (\(srcDir, modS, e) -> printf "error parsing module %s/%s: %s" (show srcDir) (show modS) (show e)) <$> errors

  return $ case length modules of
    0 -> (errorsS, Nothing)
    1 -> (errorsS, Just $ head modules)
    _ -> (errorsS ++ [printf "multiple modules found: %s" (show modules)], Nothing)

module' ::
  CabalPackage ->
  ModuleNameS ->
  (MonadLoggerIO m) => m (Maybe HsModule)
module' p mod = do
  let logTag = "parse module in package"
  (es, cm) <- module'1 p mod
  forM_ es (logErrorNSS logTag)
  return cm

---

data MS = MS
  { _msModules :: Map.Map ModuleNameS HsModule,
    _msUnparseableModules :: Set.Set ModuleNameS
  }
  deriving (Show, Generic)

$(makeLenses ''MS)

chosenModulesRecursively0 ::
  CabalPackage ->
  [ModuleNameS] ->
  (MonadLoggerIO m, MonadState MS m) => m (Set.Set ModuleNameS)
chosenModulesRecursively0 p mods = do
  modulesE <- forM mods $ \mod -> do
    cMod <- module' p mod
    return $ case cMod of
      Nothing -> Left mod
      Just m -> Right m
  let (failedModulesN', modulesN) = partitionEithers modulesE

  let failedModulesN = Set.fromList failedModulesN'
  failedModulesO <- use msUnparseableModules
  let failedModules = failedModulesN <> failedModulesO

  let modulesNM = Map.fromList $ (\m -> (_name m, m)) <$> modulesN
  modulesO <- use msModules
  let mModules = modulesNM <> modulesO

  dependencies <- concat <$> mapM module'Dependencies modulesN
  let missingModules = Set.fromList dependencies `Set.difference` Map.keysSet mModules
  let missingNonFailedModules = missingModules `Set.difference` failedModules

  msModules %= Map.union mModules
  msUnparseableModules .= failedModules
  return missingNonFailedModules

chosenModulesRecursively ::
  CabalPackage ->
  [ModuleNameS] ->
  (MonadLoggerIO m, MonadState MS m) => m (Set.Set ModuleNameS)
chosenModulesRecursively p mods = do
  let logTag = "parse given set of modules"
  missingModules <- chosenModulesRecursively0 p mods

  logDebugNSS logTag $ printf "missing modules: %s" (show missingModules)
  modsA <- use msModules
  logDebugNSS logTag $ printf "modules: %s" (show (Map.keysSet modsA))
  failedModules <- use msUnparseableModules
  logDebugNSS logTag $ printf "failed modules: %s" (show failedModules)

  if Set.null missingModules
    then return Set.empty
    else chosenModulesRecursively p (Set.toList missingModules)

---

data Package = Package
  { _ccpmodules :: Map.Map PackageNameS (Map.Map ModuleNameS HsModuleP),
    _dependencies :: [CabalPackage],
    _cabalCache :: Map.Map PackageNameS CabalPackage
  }
  deriving (Show, Generic)

$(makeLenses ''Package)

modules :: (MonadLoggerIO m, MonadState Package m) => m ()
modules = do
  let logTag = "parse packages"
  deps <- use dependencies
  st <- use ccpmodules
  logDebugNSS logTag $ printf "already parsed packages: %s" (show $ Map.keys st)
  forM_ deps $ \dep -> do
    unless (Map.member (_cabalPackageName dep) st) $ do
      logDebugNSS logTag $ printf "parsing %s..." (show $ _cabalPackageName dep)
      mods <- modules1 dep
      ccpmodules %= Map.insert (_cabalPackageName dep) mods

modules2 ::
  CabalPackage ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModule)
modules2 p = do
  let ms = MS Map.empty Set.empty
  ms' <- execStateT (chosenModulesRecursively p (Map.keys $ _cabalPackageExportedModules p)) ms
  return $ _msModules ms'

modules1 ::
  CabalPackage ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModuleP)
modules1 p = do
  mods <- modules2 p

  let iModules :: Map.Map Int HsModule = Map.fromList $ zip [1 ..] (Map.elems mods)
  let iNModules = Map.fromList $ flipTuple <$> Map.assocs (_name <$> iModules)

  let jF :: Int -> ModuleNameS -> (Int, Int)
      jF i dep = (i, fromJust $ Map.lookup dep iNModules)
      iF :: Int -> [ModuleNameS] -> [(Int, Int)]
      iF i ds = jF i <$> filter (`Map.member` iNModules) ds
  edges <- concat <$> mapM (\(i, m) -> iF i <$> module'Dependencies m) (Map.assocs iModules)
  let graph = Graph.buildG (1, Map.size mods) edges
  let graphS = Graph.reverseTopSort graph

  let oModules = (\i -> fromJust $ Map.lookup i iModules) <$> graphS

  execStateT (forM_ oModules updateExports) Map.empty

---

updateExports :: HsModule -> (MonadLoggerIO m, MonadState (Map.Map ModuleNameS HsModuleP) m) => m HsModuleP
updateExports m = do
  let logTag = "module prepare exports for " ++ _name m
  logDebugNSS logTag $ _name m

  (isImplicitExportAll, Exports {exportedVars = eV, exportedModules = eM, exportedCDs = eCD}) <- runWriterT $ haskellGetExports (_ast m)
  Imports {importedDecls = iV, importedModules = iM, importedCDs = iCD} <- execWriterT $ haskellGetImports (_ast m)
  locals <- execWriterT $ haskellGetIdentifiers (_ast m)

  st <- get
  let m' =
        if isImplicitExportAll
          then HsModuleP {_m = m, _exports = locals}
          else do
            let eM' = Map.fromList $ mapMaybe (\n -> (n,) <$> Map.lookup n st) eM
            let iM' = Map.fromList $ mapMaybe (\n -> (n,) <$> Map.lookup n st) iM

            let liCD = mapFrom (_declName . _cdtName) $ mapMaybe (enrichTryModuleCDT st) iCD <> Map.elems (_dataTypes locals) <> concatMap (Map.elems . _dataTypes . _exports) (Map.elems iM')
            let liV = asDeclsMap $ Map.elems (_decls locals) <> mapMaybe (enrichTryModule st) iV <> concatMap (Map.elems . _decls . _exports) (Map.elems iM')

            let eCDR = mapFrom (_declName . _cdtName) $ concatMap (Map.elems . _dataTypes . _exports) (Map.elems eM')
            let eVR = asDeclsMap $ concatMap (Map.elems . _decls . _exports) (Map.elems eM')

            let eCD' = mapFrom (_declName . _cdtName) eCD
            let eV' = asDeclsMap eV

            HsModuleP {_m = m, _exports = Declarations {_decls = eVR <> Map.intersection liV eV', _dataTypes = eCDR <> Map.intersection liCD eCD'}}
  modify $ Map.insert (_name m) m'
  return m'
