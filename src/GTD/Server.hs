{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server where

import Control.Lens (over, view, (%=))
import Control.Monad (forM, join, mapAndUnzipM, (<=<))
import Control.Monad.Except (MonadError (..), MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..))
import Control.Monad.State (evalStateT, modify)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Cache.LRU as LRU
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..), root)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, SourceSpan (..), asDeclsMap, emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Resolution.Module (figureOutExports1, module'Dependencies, moduleR)
import qualified GTD.Resolution.Module as Module
import GTD.Resolution.State (Context (..), Package (Package, _cabalPackage, _modules), cExports)
import qualified GTD.Resolution.State as Package
import qualified GTD.Resolution.State.Caching.Cabal as CabalCache
import qualified GTD.Resolution.State.Caching.Package as PackageCache
import GTD.Resolution.Utils (ParallelizedState (..), SchemeState (..), parallelized, scheme)
import GTD.Utils (logDebugNSS, modifyM, modifyMS)
import System.FilePath ((</>))
import System.IO (IOMode (..), stderr, stdout, withFile)
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

noDefintionFoundError :: MonadError String m => m a
noDefintionFoundError = throwError "No definition found"

---

type MS m = (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m)

---

modules :: Package -> (MS m) => m Package
modules pkg@Package {_cabalPackage = c} = do
  mods <- modules1 pkg c
  return pkg {Package._exports = Map.restrictKeys mods (Cabal._exports . Cabal._modules $ c), Package._modules = mods}

-- for a given Cabal package, it returns a list of modules in the order they should be processed
modulesOrdered :: Cabal.PackageFull -> (MS m) => m [HsModule]
modulesOrdered c = do
  flip runReaderT c $ flip evalStateT (SchemeState Map.empty Map.empty) $ do
    scheme moduleR HsModule._name id (return . module'Dependencies) (Set.toList . Cabal._exports . Cabal._modules $ c)

-- for a given Cabal package and list of its modules in the 'right' order, concurrently parses all the modules
modules1 ::
  Package ->
  Cabal.PackageFull ->
  (MS m) => m (Map.Map ModuleNameS HsModuleP)
modules1 pkg c = do
  modsO <- modulesOrdered c
  let st = ParallelizedState modsO Map.empty Map.empty (_modules pkg)
  parallelized st (Cabal.nameVersionF c) figureOutExports1 (const "tbd") HsModule._name (return . module'Dependencies)

---

package'resolution'withMutator'direct ::
  Context ->
  Cabal.PackageFull ->
  (MS m) => m (Maybe Package, Context -> Context)
package'resolution'withMutator'direct c cPkg = do
  let logTag = "package'resolution'withMutator'direct " ++ show (Cabal.nameVersionF cPkg)

  (depsC, m) <- bimap catMaybes (foldr (.) id) <$> mapAndUnzipM (PackageCache.get c <=< flip evalStateT c . CabalCache.full) (Cabal._dependencies cPkg)
  let deps = foldr (<>) Map.empty $ Package._exports <$> depsC

  pkgE <- modules $ Package {_cabalPackage = cPkg, Package._modules = deps, Package._exports = Map.empty}
  let reexports = Map.restrictKeys deps $ Cabal._reExports . Cabal._modules $ cPkg
  let pkg = pkgE {Package._exports = Package._exports pkgE <> reexports}
  PackageCache.pStore cPkg pkg

  logDebugNSS logTag $
    printf
      "given\ndeps=%s\ndepsF=%s\ndepsM=%s\nexports=%s\nreexports=%s\nPRODUCING\nexports=%s\nreexports=%s\nmodules=%s\n"
      (show $ Cabal.nameVersionP <$> Cabal._dependencies cPkg)
      (show $ Cabal.nameVersionF . _cabalPackage <$> depsC)
      (show $ Map.keys deps)
      (show $ Cabal._exports . Cabal._modules $ cPkg)
      (show $ Cabal._reExports . Cabal._modules $ cPkg)
      (show $ Map.keys $ Package._exports pkgE)
      (show $ Map.keys reexports)
      (show $ Set.difference (Map.keysSet $ Package._modules pkg) (Map.keysSet deps))
  return (Just pkg, over cExports (LRU.insert (Cabal.nameVersionF cPkg) (Package._exports pkg)) . m)

package'resolution'withMutator ::
  Context ->
  Cabal.PackageFull ->
  (MS m) => m (Maybe Package, Context -> Context)
package'resolution'withMutator c cPkg = do
  (pkgM, f) <- PackageCache.get c cPkg
  case pkgM of
    Just x -> return (Just x, f)
    Nothing -> do
      (r, m) <- package'resolution'withMutator'direct c cPkg
      return (r, m)

package'resolution ::
  Cabal.PackageFull ->
  (MS m) => m (Maybe Package)
package'resolution cPkg = do
  c <- get
  (a, m) <- package'resolution'withMutator c cPkg
  modify m
  return a

package'order'ignoringAlreadyCached :: Cabal.Package -> (MS m) => m (Maybe Cabal.PackageFull)
package'order'ignoringAlreadyCached cPkg = do b <- PackageCache.pExists cPkg; if b then return Nothing else package'order'default cPkg

package'order'default :: Cabal.Package -> (MS m) => m (Maybe Cabal.PackageFull)
package'order'default = (Just <$>) . CabalCache.full

package'dependencies'ordered ::
  Cabal.PackageFull ->
  (MS m) =>
  (Cabal.Package -> m (Maybe Cabal.PackageFull)) ->
  m [Cabal.PackageFull]
package'dependencies'ordered cPkg0 f = do
  flip evalStateT (SchemeState Map.empty Map.empty) $ do
    scheme f Cabal.nameVersionF Cabal.nameVersionP (return . Cabal._dependencies) [Cabal._fpackage cPkg0]

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
  Cabal.PackageFull ->
  (MS m) => m (Maybe Package)
package'resolution'withDependencies'concurrently cPkg0 = do
  pkgsO <- package'dependencies'ordered cPkg0 package'order'ignoringAlreadyCached
  modifyMS $ \st ->
    parallelized
      (ParallelizedState pkgsO Map.empty Map.empty st)
      ("packages", Cabal.nameVersionF cPkg0)
      package'resolution'withMutator
      package'concurrent'contextDebugInfo
      Cabal.nameVersionF
      (return . fmap Cabal.nameVersionP . Cabal._dependencies)
  package'resolution cPkg0

package'resolution'withDependencies'forked :: Cabal.PackageFull -> (MS m) => m ()
package'resolution'withDependencies'forked p = do
  let d = Cabal._path . Cabal._fpackage $ p
  r <- view root
  ec <- liftIO $
    withFile (r </> "package.stdout.log") AppendMode $ \hout -> do
      withFile (r </> "package.stderr.log") AppendMode $ \herr -> do
        (_, _, _, h) <- createProcess (proc "haskell-gtd-package" ["--dir", d]) {std_out = UseHandle hout, std_err = UseHandle herr}
        waitForProcess h
  logDebugNSS "haskell-gtd-package" $ printf "%s -> %s" d (show ec)

package ::
  Cabal.PackageFull ->
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
  cPkg <- CabalCache.findAt wd
  pkgM <- package cPkg
  pkg <- maybe (throwError "no package found?") return pkgM

  m <- parseModule emptyHsModule {_path = rf, HsModule._package = Cabal.nameF cPkg}
  resolutionMap <- fmap resolution <$> Module.resolution (_modules pkg) m

  case w `Map.lookup` resolutionMap of
    Just d -> do
      let d0 = head $ Map.elems d
      return $ DefinitionResponse {srcSpan = Just $ emptySourceSpan {sourceSpanFileName = sourceSpanFileName . _declSrcOrig $ d0, sourceSpanStartColumn = 1, sourceSpanStartLine = 1}, err = Nothing}
    Nothing -> do
      let (q, w') = fromMaybe ("", w) $ GHC.identifier w
      let look q1 w1 = join $ do
            m' <- Map.lookup q1 resolutionMap
            d <- Map.lookup w1 m'
            return $ Just DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
      let cases = if w == w' then [("", w)] else [(q, w'), ("", w)]
      casesM <- forM cases $ \(q1, w1) -> do
        let r = look q1 w1
        logDebugNSS "definition" $ printf "%s -> `%s`.`%s` -> %s" w q1 w1 (show r)
        return r
      case catMaybes casesM of
        (x : _) -> return x
        _ -> noDefintionFoundError

---

newtype DropCacheRequest = DropCacheRequest {dir :: FilePath}
  deriving (Show, Generic)

instance FromJSON DropCacheRequest

instance ToJSON DropCacheRequest

resetCache ::
  DropCacheRequest ->
  (MS m, MonadError String m) => m String
resetCache (DropCacheRequest {dir = d}) = do
  cPkg <- CabalCache.findAt d
  PackageCache.pRemove cPkg
  cExports %= fst . LRU.delete (Cabal.nameVersionF cPkg)
  return "OK"