{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GTD.Cabal.Get where

import Control.Lens (At (..), makeLenses, use, view, (%=), (.=))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.List (find)
import qualified Data.Map as Map
import Distribution.Compat.Prelude (Generic)
import GTD.Configuration (GTDConfiguration (..), repos)
import GTD.Utils (logDebugNSS')
import System.Exit (ExitCode)
import System.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))

data GetCache = GetCache
  { _vs :: Map.Map String (Maybe FilePath),
    _changed :: Bool
  }
  deriving (Show, Generic)

$(makeLenses ''GetCache)

instance FromJSON GetCache

instance ToJSON GetCache

instance Semigroup GetCache where
  (<>) :: GetCache -> GetCache -> GetCache
  (GetCache vs1 c1) <> (GetCache vs2 c2) = GetCache (vs1 <> vs2) (c1 || c2)

instance Monoid GetCache where
  mempty :: GetCache
  mempty = GetCache mempty False

get'direct :: String -> String -> (MonadIO m, MonadFail m) => m (ExitCode, Maybe String)
get'direct pkg reposR = do
  (_, Just hout, Just herr, h) <- liftIO $ createProcess (proc "cabal" ["get", pkg, "--destdir", reposR]) {std_out = CreatePipe, std_err = CreatePipe}
  stdout <- liftIO $ hGetContents hout
  stderr <- liftIO $ hGetContents herr
  let content = stdout ++ stderr
  let re = pkg ++ "-" ++ "[^\\/]*\\/"
  let packageVersion :: [String] = (=~ re) <$> lines content
  let r = find (not . null) packageVersion
  ec <- liftIO $ waitForProcess h
  return (ec, r)

-- executes `cabal get` on given `pkg + pkgVerPredicate`
-- returns version that matches given predicate
get :: String -> String -> (MonadLoggerIO m, MonadState GetCache m, MonadReader GTDConfiguration m) => MaybeT m String
get pkg pkgVerPredicate = do
  let k = pkg ++ pkgVerPredicate
  r0 <- use $ vs . at k
  case r0 of
    Just p -> MaybeT $ return p
    Nothing -> do
      reposR <- view repos
      (ec, r) <- get'direct pkg reposR
      logDebugNSS' "cabal get" $ printf "cabal get %s %s: exit code %s" pkg pkgVerPredicate (show ec)
      vs %= Map.insert k r
      changed .= True
      MaybeT $ return r