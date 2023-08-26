{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Cabal.FindAt where

import Control.Exception (catch)
import Control.Lens (At (at), use, (%=))
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..), MonadState (..))
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Distribution.Client.DistDirLayout (defaultDistDirLayout)
import Distribution.Client.HttpUtils (configureTransport)
import Distribution.Client.ProjectConfig (BadPackageLocations, ProjectPackageLocation (ProjectPackageLocalCabalFile, ProjectPackageLocalDirectory), findProjectPackages, findProjectRoot, readProjectConfig)
import Distribution.Client.RebuildMonad (runRebuild)
import Distribution.Parsec (eitherParsec)
import Distribution.Simple.Flag (Flag (..))
import Distribution.Types.CondTree (CondTree (CondNode, condTreeData))
import GTD.Cabal.Package (PackageWithUnresolvedDependencies)
import GTD.Cabal.Parse (parse)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Resolution.State (Context, ccFindAt)
import GTD.Utils (concatMapM, logDebugNSS)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)

findAt ::
  FilePath ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m [PackageWithUnresolvedDependencies]
findAt p = do
  e <- use $ ccFindAt . at p
  case e of
    Just d -> return d
    Nothing -> do
      d0 <- findAt'cabalProject p
      d <- if null d0 then findAt'regular p else return d0
      ccFindAt %= Map.insert p d
      return d

__location :: ProjectPackageLocation -> Maybe FilePath
__location (ProjectPackageLocalCabalFile p) = Just p
__location (ProjectPackageLocalDirectory _ p) = Just p
__location _ = Nothing

findAt'regular ::
  FilePath ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadError String m) => m [PackageWithUnresolvedDependencies]
findAt'regular wd = do
  cabalFiles <- liftIO $ filter (\x -> takeExtension x == ".cabal") <$> listDirectory wd
  cabalFile <- case length cabalFiles of
    0 -> throwError "No cabal file found"
    1 -> return $ wd </> head cabalFiles
    _ -> throwError "Multiple cabal files found"
  logDebugNSS "definition" $ "Found cabal file: " ++ cabalFile
  parse cabalFile

findAt'cabalProject ::
  FilePath ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m [PackageWithUnresolvedDependencies]
findAt'cabalProject wd = do
  let logTag = "findAt"
      handleError e = do
        logDebugNSS logTag $ printf "error: %s" (show e)
        throwError e

  r <- liftIO (findProjectRoot (Just wd) Nothing) >>= either (handleError . show) return
  let ddl = defaultDistDirLayout r Nothing
  v <- either handleError return $ eitherParsec "normal"
  http <- liftIO $ configureTransport v [] (Just "curl")
  CondNode {condTreeData = pc} <- liftIO $ runRebuild wd $ readProjectConfig v http NoFlag NoFlag ddl
  locs :: [ProjectPackageLocation] <- liftIO $ runRebuild wd (findProjectPackages ddl pc) `catch` (\(_ :: BadPackageLocations) -> return [])
  let locsP = (wd </>) <$> mapMaybe __location locs
  logDebugNSS logTag $ printf "%s -> %s" wd (show locsP)
  concatMapM parse locsP
