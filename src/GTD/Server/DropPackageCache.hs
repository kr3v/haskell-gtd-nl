{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import qualified GTD.Resolution.Cache as PackageCache
import GTD.Resolution.Utils (reverseDependencies)
import GTD.Server.Definition (cabalPackage, findAtF)
import GTD.State (MS, cExports, cResolution)
import GTD.Utils (mapFrom, updateStatus)
import Text.Printf (printf)

data DropPackageCacheRequest = DropCacheRequest
  { dcDir :: FilePath,
    dcFile :: FilePath
  }
  deriving (Show, Generic)

instance FromJSON DropPackageCacheRequest

instance ToJSON DropPackageCacheRequest

dropPackageCache ::
  DropPackageCacheRequest ->
  (MS m, MonadError String m) => m String
dropPackageCache (DropCacheRequest {dcDir = d, dcFile = f}) = do
  updateStatus $ printf "resetting cache on %s..." d

  cPkgs <- findAtF d
  let cPkgsM = mapFrom Cabal.key cPkgs
  cPkgU <- cabalPackage d f
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
    PackageCache.pRemove cPkg
    PackageCache.resolution'remove cPkg
    CabalCache.dropCache cPkg
    cExports %= fst . LRU.delete (Cabal.key cPkg)
  cResolution %= LRU.newLRU . LRU.maxSize
  updateStatus ""
  return "OK"