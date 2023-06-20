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
import Control.Lens (makeLenses, view, (.=), (^.))
import Control.Monad.Cont (MonadIO (..))
import Control.Monad.Logger (LoggingT (LoggingT), MonadLogger, MonadLoggerIO, logDebugN, logDebugSH, runFileLoggingT, runStdoutLoggingT)
import Control.Monad.State (MonadState, StateT (runStateT), get, gets, modify)
import Control.Monad.State.Lazy (evalStateT, execStateT, lift)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Data (Proxy (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import GHC.MVar (MVar (MVar))
import GTD.Cabal (cabalDeps, cabalPackageName, cabalPackagePath, cabalRead)
import GTD.Haskell (ContextCabalPackage (..), ContextModule (c2_identifiers), Declaration (declName, declSrcOrig), Identifier (Identifier), SourceSpan (..), dependencies, parseModule, parsePackages)
import GTD.Utils (ultraZoom)
import Network.Wai.Handler.Warp (run)
import Numeric
import Servant (HasServer (..), Server)
import Servant.API (Header, Headers, JSON, Post, ReqBody, addHeader, type (:>))
import Servant.Server (Application, Handler, hoistServer, serve)
import System.Directory (listDirectory, setCurrentDirectory)
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
  { srcSpan :: SourceSpan
  }
  deriving (Show, Generic)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

type API =
  "definition"
    :> ReqBody '[JSON] DefinitionRequest
    :> Post '[JSON] (Headers '[Header "My-Custom-Header" String] DefinitionResponse)

api :: Proxy API
api = Proxy

data ServerState = ServerState
  { _context :: ContextCabalPackage
  }

$(makeLenses ''ServerState)

type AppM = StateT (MVar ServerState) IO

type ServerStateC = MVar ServerState

nt :: ServerStateC -> AppM a -> Handler a
nt s x = liftIO (evalStateT x s)

definition :: DefinitionRequest -> (MonadIO m, MonadLogger m, MonadState ServerState m) => m DefinitionResponse
definition req@(DefinitionRequest {workDir = workDir, file = file, word = word}) = do
  liftIO $ setCurrentDirectory workDir
  files <- liftIO $ listDirectory workDir
  let cabalFiles = filter (\x -> takeExtension x == ".cabal") files

  case length cabalFiles of
    0 -> error "No cabal file found"
    1 -> do
      let cabalFile = workDir </> head cabalFiles
      logDebugN $ T.pack $ "Found cabal file: " ++ cabalFile
      pkg <- cabalRead cabalFile
      deps' <- cabalDeps pkg
      let deps = Map.elems $ Map.fromList $ (\d -> (d ^. cabalPackagePath, d)) <$> deps'

      logDebugN $ T.pack $ printf "deps list = %s\n" (show $ view cabalPackageName <$> deps)

      context . dependencies .= deps
      ultraZoom context parsePackages

      defs <- ultraZoom context (parseModule True file)
      case defs of
        Left e -> error $ "No definition found" ++ show e
        Right m -> case Identifier word `Map.lookup` c2_identifiers m of
          Nothing -> error "No definition found"
          Just d -> return DefinitionResponse {srcSpan = declSrcOrig d}
    _ -> error "Multiple cabal files found"

server :: ServerT API AppM
server req = do
  m <- get
  reqId' :: Int <- randomIO
  let reqId = showHex (abs reqId') ""
  let log = "./logs/" ++ reqId ++ ".log"
  liftIO $ print $ "Got request with ID:" ++ show reqId
  let mut :: ServerState -> IO (ServerState, DefinitionResponse)
      mut x = do
        (\(a, b) -> (b, a)) <$> runFileLoggingT log (runStateT (definition req) x)
  r <- liftIO $ modifyMVar m mut
  return $ addHeader reqId r

main :: IO ()
main = do
  print "Starting server..."
  s <- newMVar $ ServerState {_context = ContextCabalPackage {_modules = Map.empty, _dependencies = []}}
  run 8080 (serve api $ hoistServer api (nt s) server)