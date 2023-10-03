{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Resolution.Cache.Usages (get, put) where

import Control.Monad (forM_)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), asks)
import qualified Data.Binary as Binary (decodeFileOrFail, encodeFile)
import qualified Data.ByteString.Char8 as BSW8
import qualified Data.HashMap.Strict as HMap
import Data.List (isInfixOf, isPrefixOf, partition, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import GHC.Utils.Monad (mapMaybeM)
import qualified GTD.Cabal.Types as Cabal (Package (..), key, pKey)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.Cache.Utils (pathAsFile)
import GTD.Resolution.Types (Package (..), UsagesInFileMap)
import GTD.Utils (encodeWithTmp, logDebugNSS)
import System.Directory (createDirectoryIfMissing, listDirectory)
import System.FilePath (addTrailingPathSeparator, joinPath, splitPath, takeFileName, (</>))
import Text.Printf (printf)

reposMarker :: String
reposMarker = "@repos@"

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

get ::
  [Cabal.Package a] ->
  Cabal.Package a ->
  FilePath ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m [UsagesInFileMap]
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
    liftIO (Binary.decodeFileOrFail p) >>= \case
      Left (_, e) -> logDebugNSS "pGetU" e >> return Nothing
      Right x -> return $ Just x

put ::
  Cabal.Package a ->
  Package ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
put cPkg pkg = do
  logDebugNSS "pStoreU" $ printf "starting - %s (%d)" (show $ Cabal.pKey . Cabal.key $ cPkg) (HMap.size $ _usages pkg)
  forM_ (HMap.toList $ _usages pkg) $ \(f0, v) -> do
    (d, f) <- __dir'file cPkg $ BSW8.unpack f0
    liftIO $ createDirectoryIfMissing False d
    liftIO $ encodeWithTmp Binary.encodeFile (__path cPkg d f) v
  logDebugNSS "pStoreU" $ printf "done     - %s" (show $ Cabal.pKey . Cabal.key $ cPkg)
