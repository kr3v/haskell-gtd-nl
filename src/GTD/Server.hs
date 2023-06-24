{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Server where

import Control.Lens (At (at), makeLenses, use, view, (.=))
import Control.Monad.Except (MonadError (throwError), liftEither)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadIO (..), MonadReader, MonadState (..), modify)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Distribution.Compat.Directory (listDirectory)
import GHC.Generics (Generic)
import GTD.Cabal (CabalPackage, cabalDeps, cabalPackageName, cabalPackagePath, cabalRead)
import GTD.Configuration (GTDConfiguration)
import GTD.Haskell (ContextCabalPackage (..), dependencies, enrich, parsePackages)
import GTD.Haskell.Declaration (Declaration (_declSrcOrig), Identifier (Identifier), SourceSpan, hasNonEmptyOrig)
import GTD.Haskell.Module (HsModule (_decls, _path), emptyHsModule, parseModule)
import GTD.Utils (deduplicateBy, logDebugNSS, ultraZoom)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)

---

cabalDependencies' :: FilePath -> (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadError String m) => m [CabalPackage]
cabalDependencies' wd = do
  cabalFiles <- liftIO $ filter (\x -> takeExtension x == ".cabal") <$> listDirectory wd
  cabalFile <- case length cabalFiles of
    0 -> throwError "No cabal file found"
    1 -> return $ wd </> head cabalFiles
    _ -> throwError "Multiple cabal files found"
  logDebugNSS "definition" $ "Found cabal file: " ++ cabalFile
  pkg <- cabalRead cabalFile
  deduplicateBy (view cabalPackagePath) <$> cabalDeps pkg

-- FIXME: use `mtime`
data CabalCacheEntry = CabalCacheEntry
  { _mtime :: Integer,
    _deps :: [CabalPackage]
  }
  deriving (Show, Generic)

$(makeLenses ''CabalCacheEntry)

type CabalCache = Map.Map FilePath [CabalPackage]

cabalDependencies ::
  FilePath ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState CabalCache m, MonadError String m) => m [CabalPackage]
cabalDependencies wd = do
  e <- use $ at wd
  case e of
    Just d -> return d
    Nothing -> do
      d <- cabalDependencies' wd
      modify $ Map.insert wd d
      return d

data ServerState = ServerState
  { _context :: ContextCabalPackage,
    _cabalPackages :: CabalCache,
    _reqId :: Int
  }

$(makeLenses ''ServerState)

emptyServerState :: ServerState
emptyServerState = ServerState {_context = ContextCabalPackage {_ccpmodules = Map.empty, _dependencies = [], _cabalCache = Map.empty}, _cabalPackages = Map.empty, _reqId = 0}

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

noDefintionFoundErrorME :: MonadError String m => m a
noDefintionFoundErrorME = liftEither $ noDefintionFoundErrorE "No definition found"

noDefintionFoundErrorE :: (Monad m) => m (Either String a)
noDefintionFoundErrorE = runExceptT noDefintionFoundError

definition ::
  DefinitionRequest ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState ServerState m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf, word = w}) = do
  deps <- ultraZoom cabalPackages $ cabalDependencies wd

  logDebugNSS "definition" $ printf "deps list = %s" (show $ view cabalPackageName <$> deps)
  context . dependencies .= deps
  ultraZoom context parsePackages

  m <- ultraZoom context (parseModule emptyHsModule {_path = rf})
  m' <- ultraZoom context (enrich m)

  case Identifier w `Map.lookup` _decls m' of
    Nothing -> noDefintionFoundErrorME
    Just d ->
      if hasNonEmptyOrig d
        then return $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
        else noDefintionFoundErrorME