{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Resolution.Cache.Usages (get, getAll, put, remove, exists) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (MonadReader (..), asks)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BSW8
import qualified Data.HashMap.Strict as HMap
import Data.List (isInfixOf, isPrefixOf, partition, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import GHC.Utils.Monad (mapMaybeM)
import qualified GTD.Cabal.Types as Cabal (Package (..), key, pKey)
import GTD.Configuration (GTDConfiguration (..), MS0)
import GTD.Resolution.Cache.Utils (binaryGet, binaryPut, pathAsFile)
import GTD.Resolution.Module.Types
import GTD.Resolution.Types (Package (..))
import GTD.Utils (encodeWithTmp1, logDebugNSS, removeIfExists, concatForM)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (addTrailingPathSeparator, joinPath, splitPath, takeFileName, (</>))
import Text.Printf (printf)
import System.Posix (fileExist)

reposMarker :: String
reposMarker = "@repos@"

__dir'refs :: Cabal.Package a -> (MS0 m) => m FilePath
__dir'refs cPkg = (</> ((Cabal.pKey . Cabal.key $ cPkg) ++ ".refs.binary")) <$> asks _cacheUsages

-- for a given file path from a given package determine a directory and a file name in the cache/usages directory where the usages of the file should be stored
__dir'file :: Cabal.Package a -> FilePath -> (MonadReader GTDConfiguration m) => m (FilePath, FilePath)
__dir'file cPkg f = do
  c <- asks _cacheUsages

  let pr = addTrailingPathSeparator $ Cabal._projectRoot cPkg
  rr <- addTrailingPathSeparator <$> asks _repos

  let (d, fr)
        | rr `isPrefixOf` f =
            case splitPath $ fromMaybe "" $ rr `stripPrefix` f of
              [] -> ("", "")
              (x : xs) -> (x, pathAsFile $ joinPath xs ++ "." ++ reposMarker)
        | pr `isPrefixOf` f =
            (pathAsFile pr, pathAsFile $ fromMaybe "" $ pr `stripPrefix` f)
        | otherwise = ("", "")
  return (c </> d, fr)

__path :: Cabal.Package a -> FilePath -> FilePath -> FilePath
__path cPkg d p = d </> (p ++ "." ++ (Cabal.pKey . Cabal.key $ cPkg) ++ "." ++ "usages.binary")

_get1 :: FilePath -> (MS0 m) => m (Maybe UsagesPerTypeInFileMap)
_get1 = binaryGet

_put1 :: FilePath -> UsagesPerTypeInFileMap -> (MS0 m) => m ()
_put1 = binaryPut

get ::
  [Cabal.Package a] ->
  Cabal.Package a ->
  FilePath ->
  (MS0 m) => m [UsagesPerTypeInFileMap]
get cpkgsO cPkg f0 = do
  (d, f) <- __dir'file cPkg f0
  fs :: [FilePath] <- filter (f `isPrefixOf`) <$> liftIO (listDirectory d)

  let osF = Set.fromList $ (\cPkgO -> takeFileName $ __path cPkgO d f) <$> cpkgsO
  let dF = takeFileName $ __path cPkg d f

  let pO = flip Set.member osF
  let pD = (== dF)
  let pR = (reposMarker `isInfixOf`)
  let (os, rm1) = partition pO fs
  let (ds, rm2) = partition pD rm1
  let (rs, _) = partition pR rm2
  let fsA = os ++ ds ++ rs

  logDebugNSS "pGetU" $ printf "f0=%s\nd=%s\nf=%s\nfs=%s\nosF=%s\ndF=%s\nos=%s\nds=%s\nrs=%s\nfsA=%s" f0 d f (show fs) (show osF) dF (show os) (show ds) (show rs) (show fsA)

  flip mapMaybeM fsA $ \f1 -> do
    let p = d </> f1
    _get1 p

getAll ::
  Cabal.Package a ->
  FilePath ->
  (MS0 m) => m [UsagesPerTypeInFileMap]
getAll cPkg f0 = do
  (d, f) <- __dir'file cPkg f0
  fs :: [FilePath] <- filter (f `isPrefixOf`) <$> liftIO (listDirectory d)
  flip mapMaybeM fs $ \f1 -> do
    let p = d </> f1
    _get1 p

put ::
  Cabal.Package a ->
  Package ->
  (MS0 m) => m ()
put cPkg pkg = do
  logDebugNSS "pStoreU" $ printf "starting - %s (%d)" (show $ Cabal.pKey . Cabal.key $ cPkg) (HMap.size $ _usages pkg)
  paths <- concatForM (HMap.toList $ _usages pkg) $ \(f0, v) -> do
    (d, f) <- __dir'file cPkg $ BSW8.unpack f0
    let p = __path cPkg d f
    liftIO $ createDirectoryIfMissing False d
    _put1 p v
    encodeWithTmp1 JSON.encodeFile (p ++ ".json") v
    return [p, p ++ ".json"]
  __dir'refs cPkg >>= \p1 -> liftIO (binaryPut p1 paths >> encodeWithTmp1 JSON.encodeFile (p1 ++ ".json") paths)
  logDebugNSS "pStoreU" $ printf "done     - %s" (show $ Cabal.pKey . Cabal.key $ cPkg)

remove ::
  Cabal.Package a ->
  (MS0 m) => m ()
remove cPkg = do
  logDebugNSS "pRemoveU" $ printf "%s" (show $ Cabal.key cPkg)
  d <- __dir'refs cPkg
  ps <- fromMaybe [] <$> binaryGet d
  liftIO $ forM_ ps removeIfExists
  liftIO $ removeIfExists d

exists ::
  Cabal.Package a ->
  (MS0 m) => m Bool
exists cPkg = liftIO . fileExist =<< __dir'refs cPkg
