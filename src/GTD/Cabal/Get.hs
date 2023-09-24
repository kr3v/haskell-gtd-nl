{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GTD.Cabal.Get (GetCache (..), get, vs, changed) where

import Control.Concurrent (QSem, signalQSem, waitQSem)
import Control.Exception.Lifted (bracket_)
import Control.Lens (view, (%~), (.~))
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.List (find)
import qualified Data.Map as Map
import Distribution.Compat.Prelude (ExitCode (ExitFailure), fromMaybe)
import GTD.Cabal.Types (GetCache (..), changed, vs)
import GTD.Configuration (cabalGetSemaphore, repos)
import GTD.State (MS0)
import GTD.Utils (logDebugNSS)
import System.IO (hGetContents)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, proc, waitForProcess)
import Text.Printf (printf)
import Text.Regex.Posix ((=~))
import Control.Monad.Trans.Control (MonadBaseControl)

get'direct :: QSem -> String -> String -> String -> (MonadBaseControl IO m, MonadIO m, MonadFail m) => m (ExitCode, Maybe String)
get'direct s pkg pkgVerPredicate reposR = bracket_ (liftIO $ waitQSem s) (liftIO $ signalQSem s) $ do
  (_, Just hout, Just herr, h) <- liftIO $ createProcess (proc "cabal" ["get", pkg ++ pkgVerPredicate, "--destdir", reposR]) {std_out = CreatePipe, std_err = CreatePipe}
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
get ::
  GetCache ->
  String ->
  String ->
  (MS0 m) => m (Maybe String, GetCache -> GetCache)
get c pkg pkgVerPredicate = do
  let logTag = printf "cabal get %s %s" pkg pkgVerPredicate
  logDebugNSS logTag ""
  let k = pkg ++ pkgVerPredicate
  s <- view cabalGetSemaphore
  case k `Map.lookup` _vs c of
    Just p -> logDebugNSS "cabal get" (printf "cache hit for %s %s: %s" pkg pkgVerPredicate (show p)) >> return (p, id)
    Nothing -> do
      reposR <- view repos
      (ec, r) <- fromMaybe (ExitFailure 1, Nothing) <$> runMaybeT (get'direct s pkg pkgVerPredicate reposR)
      logDebugNSS logTag $ printf "cabal get: exit code %s (r = %s)" (show ec) (show r)
      return (r, (vs %~ Map.insert k r) . (changed .~ True))