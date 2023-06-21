{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, putMVar, takeMVar)
import Control.Concurrent.MVar (modifyMVar)
import Control.Lens (makeLenses, view, (.=), (<+=), (^.))
import Control.Monad.Cont (MonadIO (..))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Logger (LoggingT (LoggingT), MonadLogger, MonadLoggerIO, logDebugN, logDebugSH, runFileLoggingT, runStdoutLoggingT)
import Control.Monad.RWS (MonadIO (..), MonadReader, MonadState (get))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT (runStateT), get, gets, modify)
import Control.Monad.State.Lazy (evalStateT, execStateT, lift)
import Data.Aeson (FromJSON, ToJSON, Value)
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Data (Proxy (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import GHC.MVar (MVar (MVar))
import GTD.Cabal (cabalDeps, cabalPackageName, cabalPackagePath, cabalRead)
import GTD.Haskell (ContextCabalPackage (..), ContextModule (..), Declaration (..), Identifier (Identifier), SourceSpan (..), dependencies, parseModule, parsePackages)
import GTD.Utils (ultraZoom)
import Network.Wai.Handler.Warp (run)
import Numeric (showHex)
import Servant (HasServer (..), Server, ServerError (errBody), err400, throwError)
import Servant.API (Header, Headers, JSON, Post, ReqBody, addHeader, type (:>))
import Servant.Server (Application, Handler, hoistServer, serve)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getHomeDirectory, listDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, (</>))
import System.Random.Stateful (StdGen, mkStdGen, randomIO)
import Text.Printf (printf)

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
  deriving (Show, Generic)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

type API =
  "definition"
    :> ReqBody '[JSON] DefinitionRequest
    :> Post '[JSON] (Headers '[Header "X-Request-ID" String] DefinitionResponse)

api :: Proxy API
api = Proxy

data ServerState = ServerState
  { _context :: ContextCabalPackage,
    _reqId :: Int
  }

$(makeLenses ''ServerState)

type AppM = StateT (MVar ServerState) IO

type ServerStateC = MVar ServerState

nt :: ServerStateC -> AppM a -> Handler a
nt s x = liftIO (evalStateT x s)

definition ::
  DefinitionRequest ->
  (MonadReader ServerConstants m, MonadIO m, MonadLogger m, MonadState ServerState m) => m (Either String DefinitionResponse)
definition req@(DefinitionRequest {workDir = workDir, file = file, word = word}) = do
  files <- liftIO $ listDirectory workDir
  let cabalFiles = filter (\x -> takeExtension x == ".cabal") files

  case length cabalFiles of
    0 -> return $ Left "No cabal file found"
    1 -> do
      let cabalFile = workDir </> head cabalFiles
      logDebugN $ T.pack $ "Found cabal file: " ++ cabalFile
      pkg <- cabalRead cabalFile
      deps' <- cabalDeps pkg
      let deps = Map.elems $ Map.fromList $ (\d -> (d ^. cabalPackagePath, d)) <$> deps'

      logDebugN $ T.pack $ printf "deps list = %s" (show $ view cabalPackageName <$> deps)

      context . dependencies .= deps
      ultraZoom context parsePackages

      defs <- ultraZoom context (parseModule True file)
      case defs of
        Left e -> return $ Left $ "No definition found" ++ show e
        Right m -> case Identifier word `Map.lookup` _identifiers m of
          Nothing -> return $ Left "No definition found"
          Just d -> return $ Right $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
    _ -> return $ Left "Multiple cabal files found"

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

server :: ServerConstants -> ServerT API AppM
server c req = do
  m <- get

  rq <- liftIO $ modifyMVar m (fmap flipTuple . runStateT (reqId <+= 1))
  let reqId = printf "%06d" rq
  let log = _logs c </> (reqId ++ ".log")
  liftIO $ print $ "Got request with ID:" ++ show reqId

  r' <- liftIO $ modifyMVar m (\x -> flipTuple <$> runReaderT (runFileLoggingT log (runStateT (definition req) x)) c)
  case r' of
    Left e -> return $ addHeader reqId DefinitionResponse {srcSpan = Nothing, err = Just e}
    Right r -> return $ addHeader reqId r

data ServerConstants = ServerConstants
  { _logs :: FilePath,
    _repos :: FilePath,
    _root :: FilePath
  }
  deriving (Show)

$(makeLenses ''ServerConstants)

main :: IO ()
main = do
  print "Starting the server..."

  now <- getPOSIXTime

  homeDir <- getHomeDirectory
  let dir = homeDir </> ".local/share/haskell-gtd-extension-server-root/"
  let constants = ServerConstants {_root = dir, _logs = dir </> "logs" </> show now, _repos = dir </> "repos"}
  createDirectoryIfMissing True (constants ^. root)
  createDirectoryIfMissing True (constants ^. logs)
  createDirectoryIfMissing True (constants ^. repos)
  setCurrentDirectory (constants ^. root)

  getCurrentDirectory >>= print
  print constants

  s <- newMVar $ ServerState {_context = ContextCabalPackage {_modules = Map.empty, _dependencies = []}, _reqId = 0}
  run 8080 (serve api $ hoistServer api (nt s) (server constants))
