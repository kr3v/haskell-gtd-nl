{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server where

import Control.Lens (over, view, (%=), use, (.=))
import Control.Monad (forM, forM_, join, mapAndUnzipM, unless, void, (<=<))
import Control.Monad.Except (MonadError (..), MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..), gets)
import Control.Monad.State (evalStateT, modify)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Cache.LRU as LRU
import Data.List (find, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..), root)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, SourceSpan (..), asDeclsMap, emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule)
import qualified GTD.Haskell.Module as HsModule
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Resolution.Module (figureOutExports1, module'Dependencies, moduleR)
import GTD.Resolution.State (Context (..), Package (Package, _cabalPackage, _modules), cExports, cResolution)
import qualified GTD.Resolution.State as Package
import qualified GTD.Resolution.State.Caching.Cabal as CabalCache
import qualified GTD.Resolution.State.Caching.Package as PackageCache
import GTD.Resolution.Utils (ParallelizedState (..), SchemeState (..), parallelized, scheme)
import GTD.Utils (logDebugNSS, modifyMS, stats)
import System.FilePath (normalise, (</>))
import System.IO (IOMode (..), withFile)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Text.Printf (printf)

data DefinitionRequest = DefinitionRequest
  { workDir :: FilePath,
    file :: FilePath,
    word :: String
  }
  deriving (Show, Generic)

data DefinitionResponse = DefinitionResponse
  { srcSpan :: Maybe SourceSpan,
    err :: Maybe String
  }
  deriving (Show, Generic, Eq)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

noDefinitionFoundError :: MonadError String m => m a
noDefinitionFoundError = throwError "No definition found"

---

type MS m = (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m)

---

modules :: Package -> (MS m) => m Package
modules pkg@Package {_cabalPackage = c} = do
  mods <- modules1 pkg c
  return pkg {Package._exports = Map.restrictKeys mods (Cabal._exports . Cabal._modules $ c), Package._modules = mods}

-- for a given Cabal package, it returns a list of modules in the order they should be processed
modulesOrdered :: Cabal.PackageWithResolvedDependencies -> (MS m) => m [HsModule]
modulesOrdered c = flip runReaderT c $ flip evalStateT (SchemeState Map.empty Map.empty) $ do
  scheme moduleR HsModule._name id (return . module'Dependencies) (Set.toList . Cabal._exports . Cabal._modules $ c)

-- for a given Cabal package and list of its modules in the 'right' order, concurrently parses all the modules
modules1 ::
  Package ->
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Map.Map ModuleNameS HsModuleP)
modules1 pkg c = do
  modsO <- modulesOrdered c
  let st = ParallelizedState modsO Map.empty Map.empty (_modules pkg)
  parallelized st (Cabal.key c) figureOutExports1 (const "tbd") HsModule._name (return . module'Dependencies)

---

package'resolution'withMutator'direct ::
  Context ->
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Maybe Package, Context -> Context)
package'resolution'withMutator'direct c cPkg = do
  let logTag = "package'resolution'withMutator'direct " ++ show (Cabal.key cPkg)

  (depsC, m) <- bimap catMaybes (foldr (.) id) <$> mapAndUnzipM (PackageCache.get c <=< flip evalStateT c . CabalCache.full) (Cabal._dependencies cPkg)
  let deps = foldr (<>) Map.empty $ Package._exports <$> depsC

  pkgE <- modules $ Package {_cabalPackage = cPkg, Package._modules = deps, Package._exports = Map.empty}
  let reexports = Map.restrictKeys deps $ Cabal._reExports . Cabal._modules $ cPkg
  let pkg = pkgE {Package._exports = Package._exports pkgE <> reexports}
  PackageCache.pStore cPkg pkg

  logDebugNSS logTag $
    printf
      "given\ndeps=%s\ndepsF=%s\ndepsM=%s\nexports=%s\nreexports=%s\nPRODUCING\nexports=%s\nreexports=%s\nmodules=%s\n"
      (show $ Cabal._dependencies cPkg)
      (show $ Cabal.key . _cabalPackage <$> depsC)
      (show $ Map.keys deps)
      (show $ Cabal._exports . Cabal._modules $ cPkg)
      (show $ Cabal._reExports . Cabal._modules $ cPkg)
      (show $ Map.keys $ Package._exports pkgE)
      (show $ Map.keys reexports)
      (show $ Set.difference (Map.keysSet $ Package._modules pkg) (Map.keysSet deps))
  return (Just pkg, over cExports (LRU.insert (Cabal.key cPkg) (Package._exports pkg)) . m)

package'resolution'withMutator ::
  Context ->
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Maybe Package, Context -> Context)
package'resolution'withMutator c cPkg = do
  (pkgM, f) <- PackageCache.get c cPkg
  case pkgM of
    Just x -> return (Just x, f)
    Nothing -> do
      (r, m) <- package'resolution'withMutator'direct c cPkg
      return (r, m)

package'resolution ::
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Maybe Package)
package'resolution cPkg = do
  c <- get
  (a, m) <- package'resolution'withMutator c cPkg
  modify m
  return a

package'order'ignoringAlreadyCached :: Cabal.PackageWithUnresolvedDependencies -> (MS m) => m (Maybe Cabal.PackageWithResolvedDependencies)
package'order'ignoringAlreadyCached cPkg = do b <- PackageCache.pExists cPkg; if b then return Nothing else package'order'default cPkg

package'order'default :: Cabal.PackageWithUnresolvedDependencies -> (MS m) => m (Maybe Cabal.PackageWithResolvedDependencies)
package'order'default = (Just <$>) . CabalCache.full

package'dependencies'ordered ::
  Cabal.PackageWithUnresolvedDependencies ->
  (MS m) =>
  (Cabal.PackageWithUnresolvedDependencies -> m (Maybe Cabal.PackageWithResolvedDependencies)) ->
  m [Cabal.PackageWithResolvedDependencies]
package'dependencies'ordered cPkg0 f =
  flip evalStateT (SchemeState Map.empty Map.empty) $ do
    scheme f Cabal.key Cabal.key (return . Cabal._dependencies) [cPkg0]

package'concurrent'contextDebugInfo :: Context -> String
package'concurrent'contextDebugInfo c =
  printf
    "ccFindAt: %s, ccFull: %s, ccGet: %s, cExports: %s\nccFindAt = %s\nccFull = %s\nccGet = %s\ncExports = %s"
    (show $ Map.size $ _ccFindAt c)
    (show $ Map.size $ _ccFull c)
    (show $ Map.size $ Cabal._vs . _ccGet $ c)
    (show $ LRU.size $ _cExports c)
    (show $ Map.keys $ _ccFindAt c)
    (show $ Map.keys $ _ccFull c)
    (show $ Map.keys $ Cabal._vs . _ccGet $ c)
    (show $ fst <$> LRU.toList (_cExports c))

package'resolution'withDependencies'concurrently ::
  Cabal.PackageWithUnresolvedDependencies ->
  (MS m) => m (Maybe Package)
package'resolution'withDependencies'concurrently cPkg0 = do
  pkgsO <- package'dependencies'ordered cPkg0 package'order'ignoringAlreadyCached
  modifyMS $ \st ->
    parallelized
      (ParallelizedState pkgsO Map.empty Map.empty st)
      ("packages", Cabal.key cPkg0)
      package'resolution'withMutator
      package'concurrent'contextDebugInfo
      Cabal.key
      (return . fmap Cabal.key . Cabal._dependencies)
  CabalCache.full cPkg0 >>= package'resolution

package'resolution'withDependencies'forked :: Cabal.PackageWithResolvedDependencies -> (MS m) => m ()
package'resolution'withDependencies'forked p = do
  let d = Cabal._root p
  r <- view root
  ec <- liftIO $
    withFile (r </> "package.stdout.log") AppendMode $ \hout -> withFile (r </> "package.stderr.log") AppendMode $ \herr -> do
      (_, _, _, h) <- createProcess (proc "haskell-gtd-package" ["--dir", d]) {std_out = UseHandle hout, std_err = UseHandle herr}
      waitForProcess h
  logDebugNSS "haskell-gtd-package" $ printf "%s -> %s" d (show ec)

package ::
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Maybe Package)
package cPkg0 = do
  m <- PackageCache.pGet cPkg0
  case m of
    Just p -> return $ Just p
    Nothing -> do
      package'resolution'withDependencies'forked cPkg0
      PackageCache.pGet cPkg0

---

resolution :: Declarations -> Map.Map Identifier Declaration
resolution Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = Map.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> Map.elems (_cdtFields cd)) (Map.elems dts)
   in asDeclsMap $ ds' <> dts'

definition ::
  DefinitionRequest ->
  ((MS m), MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf, word = w}) = do
  cPkgs <- CabalCache.findAtF wd
  forM_ cPkgs $ \cPkg -> do
    e <- PackageCache.pExists cPkg
    unless e $ void $ package cPkg
  logDebugNSS "definition" $
    printf
      "rf = %s\nsource dirs=%s\n"
      rf
      (show $ (\m -> (,) (Cabal.key m) (Cabal._srcDirs . Cabal._modules $ m)) <$> cPkgs)
  let cPkgM = flip find cPkgs $ \c -> do
        any (\d -> normalise (Cabal._root c </> d) `isPrefixOf` rf) $ Cabal._srcDirs . Cabal._modules $ c
  cPkg <- maybe (throwError "cannot find a cabal 'item' with source directory that owns given file") return cPkgM

  (l, mL) <- gets $ LRU.lookup rf . _cResolution
  cResolution .= l
  resolutionMap <- case join mL of
    Just x -> return x
    Nothing -> do
      m' <- PackageCache.resolution'get emptyHsModule {_path = rf, HsModule._pkgK = Cabal.key cPkg}
      cResolution %= LRU.insert rf m'
      maybe noDefinitionFoundError return m'

  r <- case w `Map.lookup` resolutionMap of
    Just d -> do
      let d0 = head $ Map.elems $ resolution d
      return $ DefinitionResponse {srcSpan = Just $ emptySourceSpan {sourceSpanFileName = sourceSpanFileName . _declSrcOrig $ d0, sourceSpanStartColumn = 1, sourceSpanStartLine = 1}, err = Nothing}
    Nothing -> do
      let (q, w') = fromMaybe ("", w) $ GHC.identifier w
      let look q1 w1 = join $ do
            mQ <- Map.lookup q1 resolutionMap
            d <- Map.lookup w1 $ resolution mQ
            return $ Just DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
      let cases = if w == w' then [("", w)] else [(q, w'), ("", w)]
      casesM <- forM cases $ \(q1, w1) -> do
        let r = look q1 w1
        logDebugNSS "definition" $ printf "%s -> `%s`.`%s` -> %s" w q1 w1 (show r)
        return r
      case catMaybes casesM of
        (x : _) -> return x
        _ -> noDefinitionFoundError
  liftIO stats
  return r

---

newtype DropCacheRequest = DropCacheRequest {dir :: FilePath}
  deriving (Show, Generic)

instance FromJSON DropCacheRequest

instance ToJSON DropCacheRequest

resetCache ::
  DropCacheRequest ->
  (MS m, MonadError String m) => m String
resetCache (DropCacheRequest {dir = d}) = do
  cPkgs <- CabalCache.findAtF d
  forM_ cPkgs $ \cPkg -> do
    PackageCache.pRemove cPkg
    PackageCache.resolution'remove cPkg
    cExports %= fst . LRU.delete (Cabal.key cPkg)
    cResolution %= LRU.newLRU . LRU.maxSize
  return "OK"