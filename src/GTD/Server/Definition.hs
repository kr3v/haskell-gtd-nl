{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server.Definition where

import Control.Lens (use, (%=), (.=))
import Control.Monad (forM, forM_, unless, void, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (gets)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Cache.LRU as LRU
import qualified Data.HashMap.Strict as HMap
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import GHC.Utils.Monad (mapMaybeM)
import qualified GTD.Cabal.Dependencies as Cabal (fullS)
import qualified GTD.Cabal.FindAt as CabalCache (findAt)
import GTD.Cabal.Types (PackageWithResolvedDependencies, PackageWithUnresolvedDependencies)
import qualified GTD.Cabal.Types as Cabal (Dependency, Designation (..), DesignationType (..), Package (..), PackageModules (..), PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, key, pKey)
import GTD.Haskell.Declaration (Declarations (..), SourceSpan (..))
import GTD.Haskell.Module (HsModule (..), HsModuleMetadata (..), emptyHsModule, emptyMetadata)
import qualified GTD.Resolution.Cache.Package as PackageCache
import GTD.Resolution.Module.Utils (resolution'qualified, resolution'word)
import GTD.Resolution.Package (package'resolution'withDependencies'forked)
import GTD.Resolution.Types (Package (..))
import GTD.State (Context (..), MS, cLocalPackages, cResolution)
import GTD.Utils (deduplicate, logDebugNSS, peekM, stats, updateStatus)
import System.FilePath (normalise, (</>))
import Text.Printf (printf)
import qualified GTD.Resolution.Cache.Resolution as ResolutionCache

package ::
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Maybe Package)
package cPkg0 =
  PackageCache.get cPkg0 >>= \case
    Just p -> return $ Just p
    Nothing -> do
      package'resolution'withDependencies'forked cPkg0
      PackageCache.get cPkg0

package_ :: Cabal.Package a -> (MS m) => m ()
package_ cPkg0 =
  PackageCache.exists cPkg0 >>= \case
    True -> return ()
    False -> package'resolution'withDependencies'forked cPkg0

cabalPackage'unresolved'plusStoreInLocals :: FilePath -> (MS m, MonadError String m) => m [Cabal.PackageWithUnresolvedDependencies]
cabalPackage'unresolved'plusStoreInLocals f = peekM cabalPackage'contextWithLocals (CabalCache.findAt f)

cabalPackage'contextWithLocals :: (MS m) => [Cabal.PackageWithUnresolvedDependencies] -> m ()
cabalPackage'contextWithLocals cPkgsU = do
  logDebugNSS "cabalPackage'contextWithLocals" $ printf "cPkgsU = %s" (show $ Cabal.pKey . Cabal.key <$> cPkgsU)
  let libs = filter (\p -> (Cabal._desType . Cabal._designation $ p) == Cabal.Library) cPkgsU
  cLocalPackages .= mempty
  forM_ libs $ \cPkg -> do
    cLocalPackages %= Map.insertWith (<>) (Cabal._name cPkg, Cabal._desName . Cabal._designation $ cPkg) (Map.singleton (Cabal._version cPkg) cPkg)
  l <- use cLocalPackages
  logDebugNSS "cabalPackage'contextWithLocals" $ printf "cLocalPackages = %s" (show ((\(k, vs) -> (\(v, p) -> (k, v, Cabal._designation p)) <$> Map.toList vs) <$> Map.toList l))

cabalPackage'resolve :: (MS m) => [PackageWithUnresolvedDependencies] -> m [PackageWithResolvedDependencies]
cabalPackage'resolve = mapM Cabal.fullS

findAtF :: FilePath -> (MS m, MonadError String m) => m [PackageWithResolvedDependencies]
findAtF wd = cabalPackage'unresolved'plusStoreInLocals wd >>= cabalPackage'resolve

cabalPackage :: FilePath -> FilePath -> (MS m, MonadError String m) => m [PackageWithUnresolvedDependencies]
cabalPackage wd rf = do
  cPkgsU <- cabalPackage'unresolved'plusStoreInLocals wd
  if not (".hs" `isSuffixOf` rf) && not (".lhs" `isSuffixOf` rf) && not (".hs-boot" `isSuffixOf` rf) && not (".hsc" `isSuffixOf` rf)
    then return cPkgsU
    else do
      let srcDirs p = (\d -> normalise $ Cabal._root p </> d) <$> (Cabal._srcDirs . Cabal._modules $ p)
          cPkgs = filter (any (`isPrefixOf` rf) . srcDirs) cPkgsU
      when (null cPkgs) $ throwError $ "cannot find a cabal 'item' with source directory that owns file " ++ rf
      forM_ cPkgs $ \cPkg -> do
        e <- PackageCache.exists cPkg
        unless e $ void $ package_ cPkg
      logDebugNSS "cabalPackage" $ printf "cPkgs = %s" (show $ Cabal.pKey . Cabal.key <$> cPkgs)
      return cPkgs

---

data DefinitionRequest = DefinitionRequest
  { origWorkDir :: FilePath,
    workDir :: FilePath,
    file :: FilePath,
    word :: String
  }
  deriving (Show, Generic)

data DefinitionResponse = DefinitionResponse
  { srcSpan :: [SourceSpan],
    err :: Maybe String
  }
  deriving (Show, Generic, Eq)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

resolution :: (MonadLoggerIO m, MonadError String m) => HMap.HashMap String Declarations -> String -> m [SourceSpan]
resolution rm w =
  mapMaybeM
    runMaybeT
    [ resolution'qualified rm (w ++ "*"),
      resolution'qualified rm w,
      resolution'word rm w
    ]

definition ::
  DefinitionRequest ->
  (MS m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf0, word = w}) = do
  let rf = normalise rf0
  updateStatus $ printf "resolving Cabal package for %s" wd
  cPkgs <- cabalPackage wd rf

  ss <- forM cPkgs $ \cPkg -> do
    let pn = show $ Cabal.pKey . Cabal.key $ cPkg
    updateStatus $ printf "fetching 'resolution' map for %s in %s" rf pn

    let k = (Cabal.key cPkg, rf)
    (l, mL) <- gets $ LRU.lookup k . _cResolution
    cResolution .= l
    resM <- case mL of
      Just x -> return x
      Nothing -> do
        let m = emptyHsModule {_metadata = emptyMetadata {_mPath = rf, _mPkgK = Cabal.key cPkg}}
        r <- ResolutionCache.get m
        cResolution %= LRU.insert k r
        return r

    r <- case resM of
      Nothing -> return []
      Just rm -> do
        updateStatus $ printf "figuring out what `%s` is in %s" w pn
        resolution rm w
    logDebugNSS "definition" $ printf "resolution %s %s (resM = %s) = %s" w pn (show $ isJust resM) (show r)
    return r
  liftIO stats
  updateStatus ""
  return DefinitionResponse {srcSpan = deduplicate $ concat ss, err = Nothing}
