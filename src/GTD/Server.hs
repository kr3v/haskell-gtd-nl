{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module GTD.Server where

import Control.Exception.Safe (tryAny)
import Control.Lens (over, use, view, (%=), (.=))
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
import Data.List (find, intercalate, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import qualified GTD.Cabal.Cache as CabalCache
import qualified GTD.Cabal.Dependencies as Cabal (full, fullS)
import qualified GTD.Cabal.Dependencies as CabalCache (full, fullS)
import qualified GTD.Cabal.FindAt as CabalCache (findAt)
import qualified GTD.Cabal.Get as Cabal (GetCache (_vs))
import GTD.Cabal.Types (ModuleNameS, PackageWithResolvedDependencies)
import qualified GTD.Cabal.Types as Cabal (Dependency, Designation (..), Package (..), PackageModules (..), PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, key, pKey)
import GTD.Configuration (Args (..), GTDConfiguration (..), args)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, SourceSpan (..), asDeclsMap, emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule)
import qualified GTD.Haskell.Module as HsModule
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import qualified GTD.Resolution.Cache as PackageCache
import GTD.Resolution.Module (figureOutExports1, module'Dependencies, moduleR)
import GTD.Resolution.State (Context (..), Package (Package, _cabalPackage, _modules), cExports, cLocalPackages, cResolution)
import qualified GTD.Resolution.State as Package
import GTD.Resolution.Utils (ParallelizedState (..), SchemeState (..), parallelized, scheme)
import GTD.Utils (getUsableFreeMemory, logDebugNSS, modifyMS, stats, storeIOExceptionToMonadError, updateStatus)
import System.FilePath (normalise, (</>))
import System.IO (IOMode (AppendMode), withFile)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Text.Printf (printf)

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
  parallelized
    (ParallelizedState modsO Map.empty Map.empty (_modules pkg) False)
    ("modules", Cabal.key c)
    figureOutExports1
    (const "tbd")
    HsModule._name
    (return . module'Dependencies)

---

(>==>) :: Monad m => (a -> m (b, s -> s)) -> (b -> m (c, s -> s)) -> (a -> m (c, s -> s))
f >==> g = \x -> do
  (y, m1) <- f x
  (z, m2) <- g y
  return (z, m2 . m1)

(<==<) :: Monad m => (b -> m (c, s -> s)) -> (a -> m (b, s -> s)) -> (a -> m (c, s -> s))
(<==<) = flip (>==>)

package'resolution'withMutator'direct ::
  Context ->
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Maybe Package, Context -> Context)
package'resolution'withMutator'direct c cPkg = do
  let logTag = "package'resolution'withMutator'direct " ++ show (Cabal.key cPkg)

  (depsC, m) <- bimap catMaybes (foldr (.) id) <$> mapAndUnzipM (PackageCache.get c <==< CabalCache.full c) (Cabal._dependencies cPkg)
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
package'order'default = (Just <$>) . CabalCache.fullS

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
  let k = Cabal.pKey . Cabal.key $ cPkg0
  let logTag :: String = printf "%s deps" k
  updateStatus $ printf "fetching %s" logTag
  pkgsO <- package'dependencies'ordered cPkg0 package'order'ignoringAlreadyCached
  updateStatus $ printf "parsing %s..." logTag
  modifyMS $ \st ->
    parallelized
      (ParallelizedState pkgsO Map.empty Map.empty st True)
      (printf "parsing %s" logTag :: String)
      package'resolution'withMutator
      package'concurrent'contextDebugInfo
      Cabal.key
      (return . fmap Cabal.key . Cabal._dependencies)
  updateStatus $ printf "parsing %s..." k
  CabalCache.fullS cPkg0 >>= package'resolution

package'resolution'withDependencies'forked :: Cabal.PackageWithResolvedDependencies -> (MS m) => m ()
package'resolution'withDependencies'forked p = do
  let d = Cabal._root p
  Args {_dynamicMemoryUsage = dm, _logLevel = ll, _packageExe = pe, _root = r} <- view args

  let pArgs' memFree
        | memFree > 8 * 1024 = ["-N", "-A128M"]
        | memFree > 4 * 1024 = ["-N", "-A32M"]
        | memFree > 2 * 1024 = ["-N", "-A4M"]
        | otherwise = []
      pArgs = do
        memFree <- liftIO getUsableFreeMemory
        let a = pArgs' memFree
        logDebugNSS "haskell-gtd-parser" $ printf "given getUsableFreeMemory=%s and memFree=%s, rts = %s" (show dm) (show memFree) (show a)
        return a
  rts <- if dm then pArgs else return []
  let a = ["--dir", d, "--log-level", show ll] ++ if null rts then [] else ["+RTS"] ++ rts ++ ["-RTS"]

  updateStatus $ printf "executing `parser`"
  l <- liftIO $ withFile (r </> "parser.stdout.log") AppendMode $ \hout -> withFile (r </> "parser.stderr.log") AppendMode $ \herr -> do
    e <- liftIO $ tryAny $ createProcess (proc pe a) {std_out = UseHandle hout, std_err = UseHandle herr}
    x <- case e of
      Left e -> return $ show e
      Right (_, _, _, h) -> do
        x <- liftIO $ waitForProcess h
        return $ show x
    return $ printf "exe=%s args=%s -> %s" (show a) d x
  updateStatus $ printf "executing `parser` on %s... done" (Cabal.pKey . Cabal.key $ p)
  logDebugNSS "haskell-gtd-parser" l

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

---

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

cabalPackage'unresolved :: FilePath -> (MS m, MonadError String m) => m [Cabal.PackageWithUnresolvedDependencies]
cabalPackage'unresolved = CabalCache.findAt

cabalPackage'contextWithLocals :: (MS m) => [Cabal.PackageWithUnresolvedDependencies] -> m ()
cabalPackage'contextWithLocals cPkgsU = do
  cLocalPackages .= mempty
  forM_ cPkgsU $ \cPkg -> do
    cLocalPackages %= Map.insertWith (<>) (Cabal._name cPkg, Cabal._desName . Cabal._designation $ cPkg) (Map.singleton (Cabal._version cPkg) cPkg)
  l <- use cLocalPackages
  logDebugNSS "cabalPackage'contextWithLocals" $ printf "cLocalPackages = %s" (show ((\(k, vs) -> (\(v, p) -> (k, v, Cabal._designation p)) <$> Map.toList vs) <$> Map.toList l))

cabalPackage'resolve :: (MS m) => [Cabal.Package Cabal.Dependency] -> m [PackageWithResolvedDependencies]
cabalPackage'resolve = mapM Cabal.fullS

findAtF ::
  FilePath ->
  (MS m, MonadError String m) => m [PackageWithResolvedDependencies]
findAtF wd = do
  cPkgsU <- cabalPackage'unresolved wd
  cabalPackage'contextWithLocals cPkgsU
  cabalPackage'resolve cPkgsU

cabalPackage :: FilePath -> FilePath -> (MS m, MonadError String m) => m PackageWithResolvedDependencies
cabalPackage wd rf = do
  cPkgs <- findAtF wd
  let srcDirs p = (\d -> normalise $ Cabal._root p </> d) <$> (Cabal._srcDirs . Cabal._modules $ p)
      cPkgM = find (any (`isPrefixOf` rf) . srcDirs) cPkgs
  cPkg <- maybe (throwError "cannot find a cabal 'item' with source directory that owns given file") return cPkgM
  e <- PackageCache.pExists cPkg
  unless e $ void $ package cPkg
  return cPkg

definition ::
  DefinitionRequest ->
  (MS m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf0, word = w}) = do
  let rf = normalise rf0
  updateStatus $ printf "resolving Cabal package for %s" wd
  cPkg <- cabalPackage wd rf

  updateStatus $ printf "fetching 'resolution' map for %s" rf
  (l, mL) <- gets $ LRU.lookup rf . _cResolution
  cResolution .= l
  resM <- case mL of
    Just x -> return x
    Nothing -> do
      let m = emptyHsModule {_path = rf, HsModule._pkgK = Cabal.key cPkg}
      r <- PackageCache.resolution'get m
      cResolution %= LRU.insert rf r
      return r
  resolutionMap <- maybe noDefinitionFoundError return resM

  -- TODO: check both? prioritize the latter?
  updateStatus $ printf "figuring out what `%s` is" w
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

newtype DropPackageCacheRequest = DropCacheRequest {dir :: FilePath}
  deriving (Show, Generic)

instance FromJSON DropPackageCacheRequest

instance ToJSON DropPackageCacheRequest

dropPackageCache ::
  DropPackageCacheRequest ->
  (MS m, MonadError String m) => m String
dropPackageCache (DropCacheRequest {dir = d}) = do
  updateStatus $ printf "resetting cache on %s..." d
  cPkgs <- findAtF d
  forM_ cPkgs $ \cPkg -> do
    PackageCache.pRemove cPkg
    PackageCache.resolution'remove cPkg
    CabalCache.dropCache cPkg
    cExports %= fst . LRU.delete (Cabal.key cPkg)
    cResolution %= LRU.newLRU . LRU.maxSize
  updateStatus ""
  return "OK"

---

data CpphsRequest = CpphsRequest
  { crWorkDir :: FilePath,
    crFile :: FilePath
  }
  deriving (Show, Generic)

data CpphsResponse = CpphsResponse
  { crContent :: Maybe String,
    crErr :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON CpphsRequest

instance ToJSON CpphsRequest

instance FromJSON CpphsResponse

instance ToJSON CpphsResponse

cpphs ::
  CpphsRequest ->
  (MS m, MonadError String m) => m CpphsResponse
cpphs (CpphsRequest {crWorkDir = wd, crFile = rf}) = do
  updateStatus $ printf "executing `cpphs` for %s..." rf
  _ <- cabalPackage wd rf
  content <- storeIOExceptionToMonadError $ readFile rf
  r <- haskellApplyCppHs rf content
  updateStatus ""
  return $ CpphsResponse (Just r) Nothing
