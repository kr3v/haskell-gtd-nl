{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module GTD.Resolution.Module where

import Control.Monad.Cont (forM, forM_)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.State.Lazy (MonadState (..), modify)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import Control.Monad.Trans.Writer (WriterT (..), execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Distribution.ModuleName (fromString, toFilePath, validModuleComponent)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Exports (..), Imports (..), allImportedModules, asDeclsMap)
import GTD.Haskell.Module (HsModule (..), HsModuleData (..), HsModuleP (..), HsModuleParams (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Utils (logDebugNSS, logErrorNSS, mapFrom)
import System.FilePath (normalise, (</>))
import Text.Printf (printf)

---

resolve :: FilePath -> FilePath -> ModuleNameS -> FilePath
resolve repoRoot srcDir moduleName = normalise $ repoRoot </> srcDir </> ((toFilePath . fromString $ moduleName) ++ ".hs")

---

module'Dependencies :: HsModule -> [ModuleNameS]
module'Dependencies m = exportedModules (_exports0 . _info $ m) ++ allImportedModules (_imports . _info $ m)

module'2 :: Cabal.PackageFull -> ModuleNameS -> (MonadLoggerIO m) => m [Either (FilePath, ModuleNameS, String) HsModule]
module'2 p m = do
  let root = Cabal._path . Cabal._fpackage $ p
  let srcDirs = Cabal._srcDirs . Cabal._modules $ p
  forM srcDirs $ \srcDir -> runExceptT $ do
    let path = resolve root srcDir m
    logDebugNSS "module'2" $ printf "resolve(%s, %s, %s) -> %s" root srcDir m path
    let cm = emptyHsModule {HsModule._package = Cabal.nameF p, _name = m, _path = path}
    withExceptT (srcDir,m,) (parseModule cm)

module'1 :: Cabal.PackageFull -> ModuleNameS -> (MonadLoggerIO m) => m ([String], Maybe HsModule)
module'1 p m = do
  es <- module'2 p m

  let (errors, ms) = partitionEithers es
  let errorsS = (\(srcDir, modS, e) -> printf "error parsing module %s/%s: %s" (show srcDir) (show modS) (show $ take 512 e)) <$> errors

  return $ case length ms of
    0 -> (errorsS, Nothing)
    1 -> (errorsS, Just $ head ms)
    _ -> (errorsS ++ [printf "multiple modules found: %s" (show $ HsModule._name <$> ms)], Nothing)

module' :: Cabal.PackageFull -> ModuleNameS -> (MonadLoggerIO m) => m (Maybe HsModule)
module' p m = do
  let logTag = "parse module in package"
  (es, cm) <- module'1 p m
  forM_ es (logErrorNSS logTag)
  return cm

moduleR :: ModuleNameS -> (MonadLoggerIO m, MonadReader Cabal.PackageFull m) => m (Maybe HsModule)
moduleR m = do
  p <- ask
  module' p m

---

figureOutExports ::
  HsModule ->
  (MonadLoggerIO m, MonadState (Map.Map ModuleNameS HsModuleP) m) => m HsModuleP
figureOutExports m = do
  st <- get
  (r, s) <- figureOutExports0 st m
  modify s
  return r

figureOutExports0 ::
  Map.Map ModuleNameS HsModuleP ->
  HsModule ->
  (MonadLoggerIO m) => m (HsModuleP, Map.Map ModuleNameS HsModuleP -> Map.Map ModuleNameS HsModuleP)
figureOutExports0 st m = do
  let logTag = "module prepare exports for " ++ _name m
  logDebugNSS logTag $ _name m

  let (HsModuleParams {_isImplicitExportAll = isImplicitExportAll}) = _params m
      Exports {exportedVars = eV, exportedModules = eM, exportedCDs = eCD} = _exports0 . _info $ m
      Imports {importedDecls = iV, importedModules = iM, importedCDs = iCD} = _imports . _info $ m
      locals = _locals . _info $ m

  let m' =
        if isImplicitExportAll
          then HsModuleP {HsModule._exports = locals}
          else do
            let eM' = Map.fromList $ mapMaybe (\n -> (n,) <$> Map.lookup n st) eM
            let iM' = Map.fromList $ mapMaybe (\n -> (n,) <$> Map.lookup n st) iM

            let liCD = mapFrom (_declName . _cdtName) $ mapMaybe (HsModule.resolveCDT st) iCD <> Map.elems (_dataTypes locals) <> concatMap (Map.elems . _dataTypes . HsModule._exports) (Map.elems iM')
            let liV = asDeclsMap $ Map.elems (_decls locals) <> mapMaybe (HsModule.resolve st) iV <> concatMap (Map.elems . _decls . HsModule._exports) (Map.elems iM')

            let eCDR = mapFrom (_declName . _cdtName) $ concatMap (Map.elems . _dataTypes . HsModule._exports) (Map.elems eM')
            let eVR = asDeclsMap $ concatMap (Map.elems . _decls . HsModule._exports) (Map.elems eM')

            let eCD' = mapFrom (_declName . _cdtName) eCD
            let eV' = asDeclsMap eV

            HsModuleP {HsModule._exports = Declarations {_decls = eVR <> Map.intersection liV eV', _dataTypes = eCDR <> Map.intersection liCD eCD'}}
  return (m', Map.insert (_name m) m')
