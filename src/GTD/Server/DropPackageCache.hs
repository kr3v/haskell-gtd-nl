{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server.DropPackageCache where

import Control.Lens ((%=))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Cache.LRU as LRU
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import qualified GTD.Cabal.Cache as CabalCache
import qualified GTD.Cabal.Types as Cabal (Package (..), key)
import qualified GTD.Resolution.Cache.Package as PackageCache
import qualified GTD.Resolution.Cache.Resolution as ResolutionCache
import qualified GTD.Resolution.Cache.Usages as UsagesCache
import GTD.Resolution.Utils (reverseDependencies)
import GTD.Server.Definition (cabalPackage, findAtF)
import GTD.State (MS, cExports, cResolution)
import GTD.Utils (mapFrom, updateStatus)
import Text.Printf (printf)

data DropPackageCacheRequest = DropCacheRequest
  { _workDir :: FilePath,
    _file :: FilePath
  }
  deriving (Show, Generic)

instance FromJSON DropPackageCacheRequest

instance ToJSON DropPackageCacheRequest

-- dropPackageCache ensures that cache is reset when a file is saved
--
-- a saved file can correspond to one or more packages
-- all the package data should be dropped
--
-- in case this package was a library, we need to drop all the cache for its dependents,
-- because they might have used this library (and some of its symbols are likely to be invalid)
dropPackageCache ::
  DropPackageCacheRequest ->
  (MS m, MonadError String m) => m String
dropPackageCache (DropCacheRequest {_workDir = d, _file = f}) = do
  updateStatus $ printf "resetting cache on %s..." d

  cPkgs <- findAtF d
  let cPkgsM = mapFrom Cabal.key cPkgs
  cPkgsU <- cabalPackage d f
  forM_ cPkgsU $ \cPkgU -> do
    cPkg0 <- maybe (throwError "???") return $ Cabal.key cPkgU `Map.lookup` cPkgsM
    cPkgsD <-
      reverseDependencies
        (return . Just)
        Cabal.key
        Cabal.key
        (return . mapMaybe ((`Map.lookup` cPkgsM) . Cabal.key) . Cabal._dependencies)
        cPkgs
        (Cabal.key cPkg0)

    forM_ (concat cPkgsD) $ \cPkg -> do
      PackageCache.remove cPkg
      ResolutionCache.remove cPkg
      CabalCache.dropCache cPkg
      UsagesCache.remove cPkg
      cExports %= fst . LRU.delete (Cabal.key cPkg)
    cResolution %= LRU.newLRU . LRU.maxSize
    updateStatus ""
  return "OK"