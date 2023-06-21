{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Server where

import Control.Lens (makeLenses, view, (.=), (^.))
import Control.Monad.Logger (MonadLoggerIO, logDebugN)
import Control.Monad.RWS (MonadIO (..), MonadReader, MonadState)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Distribution.Compat.Directory (listDirectory)
import GHC.Generics (Generic)
import GTD.Cabal (cabalDeps, cabalPackageName, cabalPackagePath, cabalRead)
import GTD.Configuration (GTDConfiguration)
import GTD.Haskell (ContextCabalPackage (..), ContextModule (_identifiers), Declaration (_declSrcOrig), Identifier (Identifier), SourceSpan, dependencies, hasNonEmptyOrig, parseModule, parsePackages)
import GTD.Utils (deduplicateBy, ultraZoom)
import System.Directory (createDirectoryIfMissing, getHomeDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)

---

data ServerState = ServerState
  { _context :: ContextCabalPackage,
    _reqId :: Int
  }

$(makeLenses ''ServerState)

emptyServerState :: ServerState
emptyServerState = ServerState {_context = ContextCabalPackage {_modules = Map.empty, _dependencies = []}, _reqId = 0}

data DefinitionRequest = DefinitionRequest
  { workDir :: FilePath,
    file :: FilePath,
    word :: String
  }
  deriving (Show, Generic)

data DefinitionResponse = DefinitionResponse
  { srcSpan :: Maybe SourceSpan,
    err :: Maybe String
  }
  deriving (Show, Generic, Eq)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

noDefintionFoundError :: Monad m => ExceptT String m a
noDefintionFoundError = throwE "No definition found"

noDefintionFoundErrorE :: (Monad m) => m (Either String a)
noDefintionFoundErrorE = runExceptT noDefintionFoundError

definition ::
  DefinitionRequest ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState ServerState m) => ExceptT String m DefinitionResponse
definition req@(DefinitionRequest {workDir = workDir, file = file, word = word}) = do
  cabalFiles <- liftIO $ filter (\x -> takeExtension x == ".cabal") <$> listDirectory workDir
  cabalFile <- case length cabalFiles of
    0 -> throwE "No cabal file found"
    1 -> return $ workDir </> head cabalFiles
    _ -> throwE "Multiple cabal files found"
  logDebugN $ T.pack $ "Found cabal file: " ++ cabalFile

  pkg <- cabalRead cabalFile

  deps <- deduplicateBy (view cabalPackagePath) <$> cabalDeps pkg
  logDebugN $ T.pack $ printf "deps list = %s" (show $ view cabalPackageName <$> deps)
  context . dependencies .= deps
  ultraZoom context parsePackages

  m <- ExceptT $ ultraZoom context (runExceptT $ parseModule True file)
  case Identifier word `Map.lookup` _identifiers m of
    Nothing -> noDefintionFoundError
    Just d ->
      if hasNonEmptyOrig d
        then return $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
        else noDefintionFoundError