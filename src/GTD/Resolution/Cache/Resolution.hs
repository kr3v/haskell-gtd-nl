{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module GTD.Resolution.Cache.Resolution (get, put, remove, exists, __dir) where

import Control.Monad (when)
import Control.Monad.Except (MonadIO (..))
import Control.Monad.Logger (LogLevel (LevelDebug), MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), asks)
import qualified Data.Aeson as JSON
import qualified Data.Binary as Binary (Binary, encodeFile)
import qualified Data.HashMap.Strict as HMap
import Data.Maybe (isJust)
import qualified GTD.Cabal.Types as Cabal (ModuleNameS, Package (..), PackageKey, key, pKey)
import GTD.Configuration (Args (..), GTDConfiguration (..))
import GTD.Haskell.Declaration (Declarations)
import GTD.Haskell.Module (HsModule (..), metadataPrettyShow)
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.Cache.Utils (binaryGet, pathAsFile)
import GTD.Utils (encodeWithTmp, logDebugNSS)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.FilePath ((</>))
import Text.Printf (printf)

__dir :: Cabal.PackageKey -> (MonadIO m, MonadReader GTDConfiguration m) => m FilePath
__dir k = do
  c <- asks _cache
  let p = Cabal.pKey k
      d = c </> p
  liftIO $ createDirectoryIfMissing False d
  return d

__path :: String -> HsModule -> (MonadIO m, MonadReader GTDConfiguration m) => m FilePath
__path n m = do
  d <- __dir $ HsModule._pkgK m
  let r = pathAsFile $ HsModule._path m
  return $ d </> (r ++ ":" ++ n)

get'generic :: String -> HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m, Binary.Binary a) => m (Maybe a)
get'generic n m = do
  p <- __path n m
  r <- binaryGet p
  logDebugNSS "resolution get generic" $ printf "(%s,%s)@%s -> %s" (metadataPrettyShow $ _metadata m) n p (show $ isJust r)
  return r

put'generic :: JSON.ToJSON a => String -> HsModule -> (Binary.Binary a, JSON.ToJSON a) => a -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
put'generic n m r = do
  p <- __path n m
  liftIO $ encodeWithTmp Binary.encodeFile p r
  ll <- asks $ _logLevel . _args
  liftIO $ when (ll == LevelDebug) $ encodeWithTmp JSON.encodeFile (p ++ ".json") r

exists'generic :: String -> HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
exists'generic n m = do
  p <- __path n m
  liftIO $ doesFileExist p

get :: HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (Maybe (HMap.HashMap Cabal.ModuleNameS Declarations))
get = get'generic "resolution.binary"

put :: HsModule -> HMap.HashMap Cabal.ModuleNameS Declarations -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
put = put'generic "resolution.binary"

remove :: Cabal.Package a -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m ()
remove cPkg = do
  d <- __dir $ Cabal.key cPkg
  logDebugNSS "resolution cached remove" $ printf "%s @ %s" (show $ Cabal.pKey . Cabal.key $ cPkg) d
  liftIO $ removeDirectoryRecursive d

exists :: HsModule -> (MonadLoggerIO m, MonadReader GTDConfiguration m) => m Bool
exists = exists'generic "resolution.binary"
