{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Resolution.Cache where

import Control.Lens (over, view)
import Control.Monad (forM, forM_, when)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LogLevel (LevelDebug), MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..), asks, gets, modify)
import qualified Data.Aeson as JSON
import qualified Data.Binary as Binary (Binary, decodeFileOrFail, encodeFile)
import qualified Data.ByteString.Char8 as BSW8
import qualified Data.Cache.LRU as LRU
import qualified Data.HashMap.Strict as HMap
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import GHC.Utils.Monad (mapMaybeM)
import qualified GTD.Cabal.Types as Cabal (ModuleNameS, Package (..), PackageKey, PackageWithResolvedDependencies, dKey, key, pKey)
import GTD.Configuration (Args (..), GTDConfiguration (..))
import GTD.Haskell.Declaration (Declarations)
import GTD.Haskell.Lines (Lines)
import GTD.Haskell.Module (HsModule (..), metadataPrettyShow)
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.Caching.Utils (binaryGet, pathAsFile)
import GTD.Resolution.Types (Package (..), UsagesMap, UsagesInFileMap)
import qualified GTD.Resolution.Types as Package
import GTD.State (Context, cExports)
import GTD.Utils (encodeWithTmp, logDebugNSS, removeIfExistsL)
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeDirectoryRecursive)
import System.FilePath (addTrailingPathSeparator, joinPath, splitPath, takeDirectory, (</>))
import Text.Printf (printf)

exportsN :: String
exportsN = "exports.binary"

modulesN :: String
modulesN = "modules.binary"

path :: Cabal.Package a -> FilePath -> (MonadIO m, MonadReader GTDConfiguration m) => m FilePath
path cPkg f = do
  c <- asks _cache
  d <- __resolution'dir $ Cabal.key cPkg
  let r = pathAsFile $ Cabal._root cPkg
      p = Cabal.dKey . Cabal._designation $ cPkg
  return $ c </> d </> (r ++ ":" ++ p ++ ":" ++ f)

__pGet :: Cabal.Package b -> FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m, Binary.Binary a) => m (Maybe a)
__pGet cPkg f = do
  p <- path cPkg f
  binaryGet p

pExists :: Cabal.Package a -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
pExists cPkg = do
  p <- path cPkg exportsN
  r <- liftIO $ doesFileExist p
  logDebugNSS "package cached exists" $ printf "%s, %s -> %s" (show $ Cabal.pKey . Cabal.key $ cPkg) p (show r)
  return r

pGet :: Cabal.PackageWithResolvedDependencies -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Package)
pGet cPkg = do
  modulesJ <- __pGet cPkg modulesN
  exportsJ <- __pGet cPkg exportsN
  let p = Package cPkg <$> modulesJ <*> exportsJ <*> pure HMap.empty
  logDebugNSS "package cached get" $ printf "%s -> %s (%s, %s)" (show $ Cabal.pKey . Cabal.key $ cPkg) (show $ isJust p) (show $ isJust modulesJ) (show $ isJust exportsJ)
  return p

pStore ::
  Cabal.PackageWithResolvedDependencies ->
  Package ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
pStore cPkg pkg = do
  ll <- asks $ _logLevel . _args
  modulesP <- path cPkg modulesN
  exportsP <- path cPkg exportsN
  liftIO $ encodeWithTmp Binary.encodeFile modulesP (Package._modules pkg)
  liftIO $ encodeWithTmp Binary.encodeFile exportsP (Package._exports pkg)
  liftIO $ when (ll == LevelDebug) $ do
    encodeWithTmp JSON.encodeFile (modulesP ++ ".json") (Package._modules pkg)
    encodeWithTmp JSON.encodeFile (exportsP ++ ".json") (Package._exports pkg)
  logDebugNSS "package cached put" $ printf "%s -> (%d, %d)" (show $ Cabal.pKey . Cabal.key $ cPkg) (length $ Package._modules pkg) (length $ Package._exports pkg)

pRemove ::
  Cabal.Package a ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
pRemove cPkg = do
  path cPkg modulesN >>= removeIfExistsL
  path cPkg exportsN >>= removeIfExistsL
  logDebugNSS "package cached remove" $ printf "%s" (show $ Cabal.key cPkg)

get ::
  Context ->
  Cabal.PackageWithResolvedDependencies ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Package, Context -> Context)
get c cPkg = do
  let k = Cabal.key cPkg
      (_, r) = LRU.lookup k (view cExports c)
      defM = over cExports (fst . LRU.lookup k)
  logDebugNSS "package cached get'" $ printf "%s -> %s" (show $ Cabal.pKey . Cabal.key $ cPkg) (show $ isJust r)
  let p = Package cPkg mempty mempty mempty
  case r of
    Just es -> return (Just p {Package._exports = es}, defM)
    Nothing -> do
      eM <- __pGet cPkg exportsN
      return $ case eM of
        Nothing -> (Nothing, defM)
        Just e -> (Just p {Package._exports = e}, over cExports (LRU.insert k e))

setCacheMaxSize :: (MonadLoggerIO m, MonadState (LRU.LRU k v) m, Ord k) => Integer -> m ()
setCacheMaxSize n = do
  z <- gets LRU.maxSize
  case z of
    Just n0 | n0 >= n -> return ()
    _ -> do
      logDebugNSS "package cache" $ printf "setting size to %d" n
      modify $ \lru -> LRU.fromList (Just n) $ LRU.toList lru

---

pathU'h :: Cabal.Package a -> FilePath -> (MonadReader GTDConfiguration m) => m (String, String)
pathU'h cPkg f = do
  let pr = addTrailingPathSeparator $ Cabal._projectRoot cPkg
  rr <- addTrailingPathSeparator <$> asks _repos

  return $
    if rr `isPrefixOf` f
      then case splitPath $ fromMaybe "" $ rr `stripPrefix` f of
        [] -> (rr, "")
        (x : xs) -> (x, joinPath xs)
      else
        if pr `isPrefixOf` f
          then (pathAsFile pr, fromMaybe "" (pr `stripPrefix` f))
          else ("", "")

pathU :: Cabal.Package a -> FilePath -> (MonadIO m, MonadReader GTDConfiguration m) => m (FilePath, FilePath)
pathU cPkg f = do
  c <- asks _cacheUsages
  (d, fr) <- pathU'h cPkg f
  return $ (,) (c </> d) (pathAsFile fr)

pGetU :: Cabal.Package a -> FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m [UsagesInFileMap]
pGetU cPkg f0 = do
  (d, f) <- pathU cPkg f0
  fs :: [FilePath] <- filter (f `isPrefixOf`) <$> liftIO (listDirectory d)
  flip mapMaybeM fs $ \f1 -> do
    let p = d </> f1
    liftIO (Binary.decodeFileOrFail p) >>= \case
      Left (_, e) -> logDebugNSS "pGetU" e >> return Nothing
      Right x -> return $ Just x

pStoreU ::
  Cabal.Package a ->
  Package ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
pStoreU cPkg pkg = do
  logDebugNSS "pStoreU" $ printf "starting - %s (%d)" (show $ Cabal.pKey . Cabal.key $ cPkg) (HMap.size $ _usages pkg)
  forM_ (HMap.toList $ _usages pkg) $ \(p, v) -> do
    (d, p) <- pathU cPkg $ BSW8.unpack p
    liftIO $ createDirectoryIfMissing False d
    liftIO $ encodeWithTmp Binary.encodeFile (d </> (p ++ "." ++ (Cabal.pKey . Cabal.key $ cPkg) ++ "." ++ "usages.binary")) v
  logDebugNSS "pStoreU" $ printf "done     - %s" (show $ Cabal.pKey . Cabal.key $ cPkg)

---

__resolution'dir :: Cabal.PackageKey -> (MonadIO m, MonadReader GTDConfiguration m) => m FilePath
__resolution'dir k = do
  c <- asks _cache
  let p = Cabal.pKey k
      d = c </> p
  liftIO $ createDirectoryIfMissing False d
  return d

__resolution'path :: String -> HsModule -> (MonadIO m, MonadReader GTDConfiguration m) => m FilePath
__resolution'path n m = do
  d <- __resolution'dir $ HsModule._pkgK m
  let r = pathAsFile $ HsModule._path m
  return $ d </> (r ++ ":" ++ n)

resolution'get'generic :: String -> HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m, Binary.Binary a) => m (Maybe a)
resolution'get'generic n m = do
  p <- __resolution'path n m
  r <- binaryGet p
  logDebugNSS "resolution get generic" $ printf "(%s,%s)@%s -> %s" (metadataPrettyShow $ _metadata m) n p (show $ isJust r)
  return r

resolution'put'generic :: JSON.ToJSON a => String -> HsModule -> (Binary.Binary a, JSON.ToJSON a) => a -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
resolution'put'generic n m r = do
  p <- __resolution'path n m
  liftIO $ encodeWithTmp Binary.encodeFile p r
  ll <- asks $ _logLevel . _args
  liftIO $ when (ll == LevelDebug) $ encodeWithTmp JSON.encodeFile (p ++ ".json") r

resolution'exists'generic :: String -> HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
resolution'exists'generic n m = do
  p <- __resolution'path n m
  liftIO $ doesFileExist p

resolution'get :: HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe (HMap.HashMap Cabal.ModuleNameS Declarations))
resolution'get = resolution'get'generic "resolution.binary"

resolution'get'lines :: HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe Lines)
resolution'get'lines = resolution'get'generic "lines.binary"

resolution'put :: HsModule -> HMap.HashMap Cabal.ModuleNameS Declarations -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
resolution'put = resolution'put'generic "resolution.binary"

resolution'put'lines :: HsModule -> Lines -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
resolution'put'lines = resolution'put'generic "lines.binary"

resolution'remove :: Cabal.Package a -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
resolution'remove cPkg = do
  d <- __resolution'dir $ Cabal.key cPkg
  logDebugNSS "resolution cached remove" $ printf "%s @ %s" (show $ Cabal.pKey . Cabal.key $ cPkg) d
  liftIO $ removeDirectoryRecursive d

resolution'exists :: HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
resolution'exists = resolution'exists'generic "resolution.binary"

resolution'exists'lines :: HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
resolution'exists'lines = resolution'exists'generic "lines.binary"
