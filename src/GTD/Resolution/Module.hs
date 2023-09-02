{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module GTD.Resolution.Module where

import Control.Monad.Cont (forM, forM_, when)
import Control.Monad.Logger (MonadLoggerIO, NoLoggingT (runNoLoggingT))
import Control.Monad.RWS (MonadReader (ask), MonadWriter (..))
import Control.Monad.State.Lazy (MonadState (..), execStateT, modify)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import Control.Monad.Trans.Writer (execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import Distribution.ModuleName (fromString, toFilePath)
import GTD.Cabal.Types (ModuleNameS)
import qualified GTD.Cabal.Types as Cabal (Package (_modules, _name, _root), PackageModules (_srcDirs), PackageWithResolvedDependencies, key)
import GTD.Configuration (GTDConfiguration)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Module (..), ModuleImportType (..), allImportedModules, asDeclsMap)
import GTD.Haskell.Module (HsModule (..), HsModuleData (..), HsModuleP (..), HsModuleParams (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import qualified GTD.Resolution.Cache as PackageCache
import GTD.Utils (logDebugNSS, logErrorNSS, mapFrom)
import System.FilePath (normalise, (</>))
import Text.Printf (printf)

---

resolve :: FilePath -> FilePath -> ModuleNameS -> FilePath
resolve repoRoot srcDir moduleName = normalise $ repoRoot </> srcDir </> ((toFilePath . fromString $ moduleName) ++ ".hs")

---

-- this set of functions is responsible for parsing a single module (no inter-module resolution happens here) in a given Cabal package
-- its main purpose is to accumulate errors atm

module'Dependencies :: HsModule -> [ModuleNameS]
module'Dependencies m = filter (_name m /=) (allImportedModules . _imports . _info $ m)

module'2 :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m [Either (FilePath, ModuleNameS, String) HsModule]
module'2 p m = do
  let root = Cabal._root p
  let srcDirs = Cabal._srcDirs . Cabal._modules $ p
  forM srcDirs $ \srcDir -> runExceptT $ do
    let path = resolve root srcDir m
    logDebugNSS "module'2" $ printf "resolve(%s, %s, %s) -> %s" root srcDir m path
    let cm = emptyHsModule {HsModule._package = Cabal._name p, _name = m, _path = path, HsModule._pkgK = Cabal.key p}
    withExceptT (srcDir,m,) (parseModule cm)

module'1 :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m ([String], Maybe HsModule)
module'1 p m = do
  es <- module'2 p m

  let (errors, ms) = partitionEithers es
  let errorsS = (\(srcDir, modS, e) -> printf "error parsing module %s/%s: %s" (show srcDir) (show modS) (show $ take 512 e)) <$> errors

  return $ case length ms of
    0 -> (errorsS, Nothing)
    1 -> (errorsS, Just $ head ms)
    _ -> (errorsS ++ [printf "multiple modules found: %s" (show $ HsModule._name <$> ms)], Nothing)

module' :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m (Maybe HsModule)
module' p m = do
  let logTag = "parse module in package"
  (es, cm) <- module'1 p m
  forM_ es (logErrorNSS logTag)
  return cm

moduleR :: ModuleNameS -> (MonadLoggerIO m, MonadReader Cabal.PackageWithResolvedDependencies m) => m (Maybe HsModule)
moduleR m = do
  p <- ask
  module' p m

---

resolution ::
  Map.Map ModuleNameS HsModuleP ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Map.Map ModuleNameS Declarations)
resolution sM m = do
  x <- runNoLoggingT $ PackageCache.resolution'get m
  case x of
    Just r -> return r
    Nothing -> do
      r <- resolution'direct sM m
      PackageCache.resolution'put m r
      return r

resolution'direct ::
  Map.Map ModuleNameS HsModuleP ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Map.Map ModuleNameS Declarations)
resolution'direct sM m = flip execStateT Map.empty $ do
  let locals = _locals . _info $ m
      name = _name m

  forM_ (_imports . _info $ m) $ \Module {_mName = k, _mType = mt, _mAllowNoQualifier = mnq, _mQualifier = mq, _mDecls = md, _mCDs = mc} -> forM_ (Map.lookup k sM) $ \c -> do
    let stuff = case mt of
          All -> _exports c
          EverythingBut -> Declarations {_decls = Map.withoutKeys (_decls $ _exports c) (Map.keysSet $ asDeclsMap md), _dataTypes = Map.withoutKeys (_dataTypes $ _exports c) (Map.keysSet $ mapFrom (_declName . _cdtName) mc)}
          Exactly -> Declarations {_decls = Map.intersection (_decls $ _exports c) (asDeclsMap md), _dataTypes = Map.intersection (_dataTypes $ _exports c) (mapFrom (_declName . _cdtName) mc)}
    modify $ Map.insertWith (<>) mq stuff
    when mnq $ modify $ Map.insertWith (<>) "" stuff
  modify $ Map.insertWith (<>) name locals
  modify $ Map.insertWith (<>) "" locals

---

figureOutExports ::
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState (Map.Map ModuleNameS HsModuleP) m) => m HsModuleP
figureOutExports m = do
  st <- get
  (r, s, _) <- figureOutExports0 st m
  modify s
  return r

figureOutExports1 ::
  Map.Map ModuleNameS HsModuleP ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (HsModuleP, Map.Map ModuleNameS HsModuleP -> Map.Map ModuleNameS HsModuleP)
figureOutExports1 sM m = do
  (r, s, _) <- figureOutExports0 sM m
  return (r, s)

figureOutExports0 ::
  Map.Map ModuleNameS HsModuleP ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (HsModuleP, Map.Map ModuleNameS HsModuleP -> Map.Map ModuleNameS HsModuleP, Map.Map ModuleNameS Declarations)
figureOutExports0 sM m = do
  liM <- resolution sM m
  r <-
    execWriterT $
      if _isImplicitExportAll . _params $ m
        then tell $ _locals . _info $ m
        else forM_ (Map.toList $ _exports0 . _info $ m) $ \(k, e) -> do
          let n = _name m
          if _mType e == All
            then
              if k == n
                then tell $ _locals . _info $ m
                else forM_ (Map.lookup k sM) $ \c -> tell (_exports c)
            else forM_ (Map.lookup k liM) $ \c -> do
              let eD = Declarations {_decls = Map.intersection (_decls c) (asDeclsMap $ _mDecls e), _dataTypes = Map.intersection (_dataTypes c) (mapFrom (_declName . _cdtName) $ _mCDs e)}
              tell eD

  return (HsModuleP {HsModule._exports = r}, Map.insert (_name m) (HsModuleP {_exports = r}), liM)