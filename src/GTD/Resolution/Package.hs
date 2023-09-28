{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Resolution.Package where

import Control.DeepSeq (deepseq)
import Control.Exception.Safe (tryAny)
import Control.Lens (over, view)
import Control.Monad (mapAndUnzipM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (MonadState (..))
import Control.Monad.State (modify)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.Cache.LRU as LRU
import qualified Data.HashMap.Strict as HMap
import Data.List (singleton)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified GTD.Cabal.Dependencies as CabalCache (full, fullS)
import qualified GTD.Cabal.Types as Cabal (Designation (..), GetCache (_vs), Package (..), PackageModules (..), PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, key, pKey)
import GTD.Configuration (Args (..), args)
import qualified GTD.Resolution.Cache as PackageCache
import GTD.Resolution.Module (modules)
import GTD.Resolution.Types (Package (..))
import qualified GTD.Resolution.Types as Package
import GTD.Resolution.Utils (ParallelizedState (..), parallelized, scheme)
import GTD.State (Context (..), MS, MS0, cExports)
import GTD.Utils (logDebugNSS, modifyMS, updateStatus, (<==<), restrictKeys)
import System.FilePath ((</>))
import System.IO (IOMode (AppendMode), withFile)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import qualified Data.HashSet as HSet
import GTD.Utils.OS.Memory (availableMemory)

package'resolution'withMutator'direct ::
  Context ->
  Cabal.PackageWithResolvedDependencies ->
  (MS0 m) => m (Context -> Context)
package'resolution'withMutator'direct c cPkg = do
  let logTag = "package'resolution'withMutator'direct " ++ show (Cabal.pKey . Cabal.key $ cPkg)

  (depsC, m) <- bimap catMaybes (foldr (.) id) <$> mapAndUnzipM (PackageCache.get c <==< CabalCache.full c) (Cabal._dependencies cPkg)
  let deps = foldr (<>) HMap.empty $ Package._exports <$> depsC

  pkgE <- modules $ Package {_cabalPackage = cPkg, Package._modules = deps, Package._exports = HMap.empty, Package._usages = HMap.empty}
  let reexports = restrictKeys deps $ Cabal._reExports . Cabal._modules $ cPkg
  let pkg = pkgE {Package._exports = Package._exports pkgE <> reexports}
  PackageCache.pStore cPkg pkg
  PackageCache.pStoreU cPkg pkg

  logDebugNSS logTag $
    printf
      "given\ndeps=%s\ndepsF=%s\ndepsM=%s\nexports=%s\nreexports=%s\nPRODUCING\nexports=%s\nreexports=%s\nmodules=%s\n"
      (show $ Cabal._dependencies cPkg)
      (show $ Cabal.key . _cabalPackage <$> depsC)
      (show $ HMap.keys deps)
      (show $ Cabal._exports . Cabal._modules $ cPkg)
      (show $ Cabal._reExports . Cabal._modules $ cPkg)
      (show $ HMap.keys $ Package._exports pkgE)
      (show $ HMap.keys reexports)
      (show $ HSet.difference (HMap.keysSet $ Package._modules pkg) (HMap.keysSet deps))
  return (over cExports (LRU.insert (Cabal.key cPkg) (Package._exports pkg)) . m)

package'resolution'withMutator ::
  Context ->
  Cabal.PackageWithResolvedDependencies ->
  (MS0 m) => m (Context -> Context)
package'resolution'withMutator c cPkg = do
  (pkgM, f) <- PackageCache.get c cPkg
  case pkgM of
    Just _ -> return f
    Nothing -> package'resolution'withMutator'direct c cPkg

package'resolution ::
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m ()
package'resolution cPkg = do
  c <- get
  m <- package'resolution'withMutator c cPkg
  modify m

package'order'ignoringAlreadyCached :: Cabal.PackageWithUnresolvedDependencies -> (MS m) => m (Maybe Cabal.PackageWithResolvedDependencies)
package'order'ignoringAlreadyCached cPkg = do b <- PackageCache.pExists cPkg; if b then return Nothing else package'order'default cPkg

package'order'default :: Cabal.PackageWithUnresolvedDependencies -> (MS m) => m (Maybe Cabal.PackageWithResolvedDependencies)
package'order'default = (Just <$>) . CabalCache.fullS

package'dependencies'ordered ::
  Cabal.PackageWithUnresolvedDependencies ->
  (MS m) =>
  (Cabal.PackageWithUnresolvedDependencies -> m (Maybe Cabal.PackageWithResolvedDependencies)) ->
  m [Cabal.PackageWithResolvedDependencies]
package'dependencies'ordered cPkg0 f = scheme f Cabal.key Cabal.key (return . Cabal._dependencies) [cPkg0]

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
  (MS m) => m ()
package'resolution'withDependencies'concurrently cPkg0 = do
  let k = Cabal.pKey . Cabal.key $ cPkg0
  let logTag :: String = printf "%s deps" k
  updateStatus $ printf "fetching %s" logTag
  pkgsO <- package'dependencies'ordered cPkg0 package'order'ignoringAlreadyCached
  updateStatus $ printf "parsing %s..." logTag
  modifyMS $ \st ->
    parallelized
      (ParallelizedState pkgsO Set.empty Map.empty st True)
      (printf "parsing %s" logTag :: String)
      package'resolution'withMutator
      package'concurrent'contextDebugInfo
      Cabal.key
      (return . fmap Cabal.key . Cabal._dependencies)
  updateStatus $ printf "parsing %s..." k
  CabalCache.fullS cPkg0 >>= package'resolution

---

package'resolution'withDependencies'forked :: Cabal.Package a -> (MS0 m) => m ()
package'resolution'withDependencies'forked p = do
  let d = Cabal._projectRoot p
  Args {_dynamicMemoryUsage = dm, _logLevel = ll, _parserExe = pe, _parserArgs = pa, _root = r} <- view args

  let pArgs' memFree
        | memFree > 8 * 1024 = ["-N", "-A128M"]
        | memFree > 4 * 1024 = ["-N", "-A64M"]
        | memFree > 2 * 1024 = ["-N", "-A16M"]
        | otherwise = ["-N", "-A4M"]
      pArgs = do
        memFree <- liftIO availableMemory
        let a = pArgs' memFree
        logDebugNSS "haskell-gtd-nl-parser" $ printf "given getUsableFreeMemory=%s and memFree=%s, rts = %s" (show dm) (show memFree) (show a)
        return a
  rts <- if dm then pArgs else return []
  let a =
        concat
          [ pa,
            if null rts then [] else ["+RTS"] ++ rts ++ ["-RTS"],
            ["--dir", d, "--log-level", show ll, "--designation-type", (show . Cabal._desType . Cabal._designation) p],
            maybe [] ((["--designation-name"] ++) . singleton) (Cabal._desName . Cabal._designation $ p)
          ]

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
  logDebugNSS "haskell-gtd-nl-parser" l