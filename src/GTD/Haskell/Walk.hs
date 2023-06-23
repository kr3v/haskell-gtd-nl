{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Walk where

import Control.Exception (IOException)
import Control.Lens (makeLenses, use, (%=), (.=), (^.))
import Control.Monad.Cont (MonadIO, forM_)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.State (MonadIO, MonadState, MonadTrans (lift), forM)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import Control.Monad.Trans.Writer (execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (CabalLibSrcDir, CabalPackage (_cabalPackageExportedModules, _cabalPackagePath, _cabalPackageSrcDirs), ModuleNameS, cabalPackageName, haskellPath)
import GTD.Haskell.AST (haskellGetImportedModules, haskellGetReexportedModules)
import GTD.Haskell.Module (HsModule (_ast, _deps, _name, _package, _path), emptyHsModule, parseModule)
import GTD.Utils (logErrorNSS, logDebugNSS)
import Text.Printf (printf)

parseModuleInPackage'' ::
  CabalPackage ->
  ModuleNameS ->
  (MonadLoggerIO m) => m [Either (CabalLibSrcDir, ModuleNameS, String) HsModule]
parseModuleInPackage'' p mod = do
  let mods = _cabalPackageExportedModules p
  let root = _cabalPackagePath p
  let srcDirs = _cabalPackageSrcDirs p
  forM srcDirs $ \srcDir -> runExceptT $ do
    let path = haskellPath root srcDir mod
    let cm = emptyHsModule {_package = p ^. cabalPackageName, _name = mod, _path = path}
    r <- withExceptT (srcDir,mod,) (parseModule cm)
    eM <- lift $ execWriterT $ haskellGetReexportedModules (_ast r)
    iM <- lift $ execWriterT $ haskellGetImportedModules (_ast r)
    return r {_deps = eM ++ iM}

parseModuleInPackage' ::
  CabalPackage ->
  ModuleNameS ->
  (MonadLoggerIO m) => m ([String], Maybe HsModule)
parseModuleInPackage' p mod = do
  es <- parseModuleInPackage'' p mod

  let (errors, modules) = partitionEithers es
  let errorsS = (\(srcDir, modS, e) -> printf "error parsing module %s/%s: %s" (show srcDir) (show modS) (show e)) <$> errors

  return $ case length modules of
    0 -> (errorsS, Nothing)
    1 -> (errorsS, Just $ head modules)
    _ -> (errorsS ++ [printf "multiple modules found: %s" (show modules)], Nothing)

parseModuleInPackage ::
  CabalPackage ->
  ModuleNameS ->
  (MonadLoggerIO m) => m (Maybe HsModule)
parseModuleInPackage p mod = do
  let logTag = "parse module in package"
  (es, cm) <- parseModuleInPackage' p mod
  forM_ es (logErrorNSS logTag)
  return cm

---

data MS = MS
  { _modules :: Map.Map ModuleNameS HsModule,
    _unparseableModules :: Set.Set ModuleNameS
  }
  deriving (Show, Generic)

$(makeLenses ''MS)

parseChosenModules0 ::
  CabalPackage ->
  [ModuleNameS] ->
  (MonadLoggerIO m, MonadState MS m) => m (Set.Set ModuleNameS)
parseChosenModules0 p mods = do
  let root = _cabalPackagePath p
  let srcDirs = _cabalPackageSrcDirs p

  modulesE <- forM mods $ \mod -> do
    cMod <- parseModuleInPackage p mod
    return $ case cMod of
      Nothing -> Left mod
      Just m -> Right m
  let (failedModulesN', modulesN) = partitionEithers modulesE

  let failedModulesN = Set.fromList failedModulesN'
  failedModulesO <- use unparseableModules
  let failedModules = failedModulesN <> failedModulesO

  let modulesNM = Map.fromList $ (\m -> (_name m, m)) <$> modulesN
  modulesO <- use modules
  let mModules = modulesNM <> modulesO

  let missingModules = Set.fromList (concatMap _deps modulesN) `Set.difference` Map.keysSet mModules
  let missingNonFailedModules = missingModules `Set.difference` failedModules

  modules %= Map.union mModules
  unparseableModules .= failedModules
  return missingNonFailedModules

parseChosenModules ::
  CabalPackage ->
  [ModuleNameS] ->
  (MonadLoggerIO m, MonadState MS m) => m (Set.Set ModuleNameS)
parseChosenModules p mods = do
  let logTag = "parse given set of modules"
  missingModules <- parseChosenModules0 p mods

  logDebugNSS logTag $ printf "missing modules: %s" (show missingModules)
  modules <- use modules
  logDebugNSS logTag $ printf "modules: %s" (show (Map.keysSet modules))
  failedModules <- use unparseableModules
  logDebugNSS logTag $ printf "failed modules: %s" (show failedModules)

  if Set.null missingModules
    then return Set.empty
    else parseChosenModules p (Set.toList missingModules)
