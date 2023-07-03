{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Resolution where

import Control.Lens (makeLenses, use, (%=))
import Control.Monad.Cont (MonadTrans (lift), forM, forM_)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (ask), gets)
import Control.Monad.State.Lazy (MonadState (..), StateT, modify)
import Control.Monad.Trans.Except (runExceptT, withExceptT)
import Control.Monad.Trans.Writer (WriterT (..), execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Distribution.ModuleName (fromString, toFilePath)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Haskell.AST (ClassOrData (..), Declarations (..), Exports (..), Imports (..), allImportedModules, haskellGetExports, haskellGetIdentifiers, haskellGetImports)
import GTD.Haskell.Declaration (Declaration (..))
import GTD.Haskell.Enrich (enrichTryModule, enrichTryModuleCDT)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (flipTuple, logDebugNSS, logErrorNSS, mapFrom)
import System.FilePath (normalise, (</>))
import Text.Printf (printf)

---

data SchemeState k a b = SchemeState
  { _schemeStateA :: Map.Map k a,
    _schemeStateB :: Map.Map k b
  }

$(makeLenses ''SchemeState)

-- | `a` is a builder argument, that is called at most once to build a value for a key
-- | `k` is a key produced for a value
-- | `p` is a function that produces dependencies for a value
-- | `ks` is a set of keys to start with (root set)
-- | `scheme` recursively figures out dependencies for a given root set;
-- |          it is safe in terms of cyclic dependencies - `a` is called at most once for a given key
scheme ::
  (Ord k, Monad m) =>
  (b -> m (Maybe a)) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  StateT (SchemeState k a b) m ()
scheme a k p ks = do
  z <- lift $ catMaybes <$> mapM (\x -> fmap (x,) <$> a x) ks
  let (bs1, as1) = unzip z
  let ks1 = k <$> bs1
  schemeStateA %= Map.union (Map.fromList $ zip ks1 as1)
  schemeStateB %= Map.union (Map.fromList $ zip ks1 bs1)
  deps <- lift $ concat <$> mapM p as1
  let ds = mapFrom k deps
  _ks <- use schemeStateB
  let ds' = ds `Map.difference` _ks
  if Map.null ds'
    then return ()
    else scheme a k p (Map.elems ds')

scheme2 ::
  (Ord k, Monad m) =>
  (b -> m (Maybe a)) ->
  (a -> k) ->
  (b -> k) ->
  (a -> m [b]) ->
  [b] ->
  StateT (SchemeState k a b) m [a]
scheme2 a ka kb p ks = do
  scheme a kb p ks
  as <- gets _schemeStateA

  let iModules = Map.fromList $ zip [1 ..] (Map.elems as)
  let iNModules = Map.fromList $ flipTuple <$> Map.assocs (ka <$> iModules)

  let jF i dep = (i, fromJust $ Map.lookup dep iNModules)
      iF i ds = jF i <$> filter (`Map.member` iNModules) ds
  edges <- lift $ concat <$> mapM (\(i, m) -> iF i <$> ((kb <$>) <$> p m)) (Map.assocs iModules)
  let graph = Graph.buildG (1, Map.size as) edges
  let graphS = Graph.reverseTopSort graph

  return $ fromJust . (`Map.lookup` iModules) <$> graphS

---

resolve :: FilePath -> FilePath -> ModuleNameS -> FilePath
resolve repoRoot srcDir moduleName = normalise $ repoRoot </> srcDir </> ((toFilePath . fromString $ moduleName) ++ ".hs")

---

module'Dependencies :: HsModule -> (MonadLoggerIO m) => m [ModuleNameS]
module'Dependencies m = do
  eM <- execWriterT $ haskellGetExports (_ast m)
  iM <- execWriterT $ haskellGetImports (_ast m)
  return $ exportedModules eM ++ allImportedModules iM

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
  let errorsS = (\(srcDir, modS, e) -> printf "error parsing module %s/%s: %s" (show srcDir) (show modS) (show e)) <$> errors

  return $ case length ms of
    0 -> (errorsS, Nothing)
    1 -> (errorsS, Just $ head ms)
    _ -> (errorsS ++ [printf "multiple modules found: %s" (show ms)], Nothing)

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

updateExports ::
  HsModule ->
  (MonadLoggerIO m, MonadState (Map.Map ModuleNameS HsModuleP) m) => m HsModuleP
updateExports m = do
  let logTag = "module prepare exports for " ++ _name m
  logDebugNSS logTag $ _name m

  (isImplicitExportAll, Exports {exportedVars = eV, exportedModules = eM, exportedCDs = eCD}) <- runWriterT $ haskellGetExports (_ast m)
  Imports {importedDecls = iV, importedModules = iM, importedCDs = iCD} <- execWriterT $ haskellGetImports (_ast m)
  locals <- execWriterT $ haskellGetIdentifiers (_ast m)

  st <- get
  let m' =
        if isImplicitExportAll
          then HsModuleP {HsModule._exports = locals}
          else do
            let eM' = Map.fromList $ mapMaybe (\n -> (n,) <$> Map.lookup n st) eM
            let iM' = Map.fromList $ mapMaybe (\n -> (n,) <$> Map.lookup n st) iM

            let liCD = mapFrom (_declName . _cdtName) $ mapMaybe (enrichTryModuleCDT st) iCD <> Map.elems (_dataTypes locals) <> concatMap (Map.elems . _dataTypes . HsModule._exports) (Map.elems iM')
            let liV = asDeclsMap $ Map.elems (_decls locals) <> mapMaybe (enrichTryModule st) iV <> concatMap (Map.elems . _decls . HsModule._exports) (Map.elems iM')

            let eCDR = mapFrom (_declName . _cdtName) $ concatMap (Map.elems . _dataTypes . HsModule._exports) (Map.elems eM')
            let eVR = asDeclsMap $ concatMap (Map.elems . _decls . HsModule._exports) (Map.elems eM')

            let eCD' = mapFrom (_declName . _cdtName) eCD
            let eV' = asDeclsMap eV

            HsModuleP {HsModule._exports = Declarations {_decls = eVR <> Map.intersection liV eV', _dataTypes = eCDR <> Map.intersection liCD eCD'}}
  modify $ Map.insert (_name m) m'
  return m'
