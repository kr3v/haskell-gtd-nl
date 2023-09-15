{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Resolution.Module where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Cont (forM, forM_, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLoggerIO, NoLoggingT (runNoLoggingT))
import Control.Monad.RWS (MonadReader (ask), MonadWriter (..))
import Control.Monad.State.Lazy (MonadState (..), execStateT, modify)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Writer (execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Distribution.ModuleName (fromString, toFilePath)
import GTD.Cabal.Types (ModuleNameS)
import qualified GTD.Cabal.Types as Cabal (Package (_modules, _name, _root), PackageModules (_allKnownModules, _srcDirs), PackageWithResolvedDependencies, key)
import GTD.Configuration (GTDConfiguration)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Module (..), ModuleImportType (..), SourceSpan (..), allImportedModules, asDeclsMap, emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleData (..), HsModuleMetadata (HsModuleMetadata), HsModuleP (..), HsModuleParams (..), parseModule, _name)
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
module'Dependencies m = filter (HsModule._name m /=) (allImportedModules . _imports . HsModule._info $ m)

module'2 :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m [Either String HsModule]
module'2 p m = do
  let root = Cabal._root p
  let srcDirs = Cabal._srcDirs . Cabal._modules $ p
  forM srcDirs $ \srcDir -> runExceptT $ do
    let path = resolve root srcDir m
    logDebugNSS "module'2" $ printf "resolve(%s, %s, %s) -> %s" root srcDir m path
    let cm = HsModuleMetadata {HsModule._mPackage = Cabal._name p, HsModule._mName = m, HsModule._mPath = path, HsModule._mPkgK = Cabal.key p}
    parseModule cm
      `catchError` \e1 ->
        parseModule (cm {HsModule._mPath = path ++ "c"})
          `catchError` \e2 -> throwError $ printf "error parsing module %s/%s: (%s, %s)" (show srcDir) (show m) (show e1) (show e2)

module'1 :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m ([String], Maybe HsModule)
module'1 p mn = do
  (errs, ms) <- partitionEithers <$> module'2 p mn
  let shouldBePresent = Set.member mn (Cabal._allKnownModules . Cabal._modules $ p)
  return $ case ms of
    [] -> (if shouldBePresent then errs else [], Nothing)
    [m] -> (errs, Just m)
    _ -> (errs ++ [printf "multiple modules found: %s" (show $ liftA2 (,) HsModule._name HsModule._path <$> ms)], Nothing)

module' :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m (Maybe HsModule)
module' p m = do
  (es, cm) <- module'1 p m
  forM_ es $ logErrorNSS "parse module in package"
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

  forM_ (_imports . _info $ m) $
    \Module {_mName = k, _mType = mt, _mAllowNoQualifier = mnq, _mQualifier = mq, _mDecls = md, _mCDs = mc} ->
      forM_ (Map.lookup k sM) $ \c -> do
        modify $ Map.insertWith (<>) (k ++ "*") mempty {_decls = Map.singleton "" Declaration {_declModule = k, _declName = "", _declSrcOrig = emptySourceSpan {sourceSpanFileName = HsModule._mPath . HsModule._ometadata $ c, sourceSpanStartColumn = 1, sourceSpanStartLine = 1}}}
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
                else forM_ (Map.lookup k liM) $ \c -> tell c
            else forM_ (Map.lookup k liM) $ \c -> do
              let eD = Declarations {_decls = Map.intersection (_decls c) (asDeclsMap $ _mDecls e), _dataTypes = Map.intersection (_dataTypes c) (mapFrom (_declName . _cdtName) $ _mCDs e)}
              tell eD

  let x = HsModuleP {HsModule._exports = r, HsModule._ometadata = _metadata m}
  return (x, Map.insert (_name m) x, liM)