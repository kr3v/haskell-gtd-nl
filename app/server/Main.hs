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
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.Logger (LoggingT (LoggingT), MonadLogger, MonadLoggerIO, logDebugN, logDebugSH, runFileLoggingT, runStdoutLoggingT)
import Control.Monad.RWS (MonadIO (..), MonadReader, MonadState (get))
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (MonadState, StateT (runStateT), get, gets, modify)
import Control.Monad.State.Lazy (evalStateT, execStateT, lift)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
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
import GTD.Configuration
import GTD.Haskell (ContextCabalPackage (..), ContextModule (..), Declaration (..), Identifier (Identifier), SourceSpan (..), dependencies, parseModule, parsePackages)
import GTD.Server (DefinitionRequest, DefinitionResponse (..), ServerState (..), definition, reqId)
import GTD.Utils (deduplicateBy, ultraZoom)
import Network.Wai.Handler.Warp (run)
import Numeric (showHex)
import Servant (HasServer (..), Server, ServerError (errBody), err400, throwError)
import Servant.API (Header, Headers, JSON, Post, ReqBody, addHeader, type (:>))
import Servant.Server (Application, Handler, hoistServer, serve)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getHomeDirectory, listDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, (</>))
import System.Random.Stateful (StdGen, mkStdGen, randomIO)
import Text.Printf (printf)

type API =
  "definition"
    :> ReqBody '[JSON] DefinitionRequest
    :> Post '[JSON] (Headers '[Header "X-Request-ID" String] DefinitionResponse)

api :: Proxy API
api = Proxy

type AppM = StateT (MVar ServerState) IO

type ServerStateC = MVar ServerState

nt :: ServerStateC -> AppM a -> Handler a
nt s x = liftIO (evalStateT x s)

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

server :: GTDConfiguration -> ServerT API AppM
server c req = do
  m <- get

  rq <- liftIO $ modifyMVar m (fmap flipTuple . runStateT (reqId <+= 1))
  let reqId = printf "%06d" rq
  let log = _logs c </> (reqId ++ ".log")
  liftIO $ putStrLn $ "Got request with ID:" ++ show reqId

  r' <- liftIO $ modifyMVar m (\x -> flipTuple <$> runReaderT (runFileLoggingT log (runStateT (runExceptT $ definition req) x)) c)
  case r' of
    Left e -> return $ addHeader reqId DefinitionResponse {srcSpan = Nothing, err = Just e}
    Right r -> return $ addHeader reqId r

main :: IO ()
main = do
  putStrLn "Starting the server..."

  constants <- prepareConstants
  setCurrentDirectory (constants ^. root)
  getCurrentDirectory >>= print
  print constants

  s <- newMVar $ ServerState {_context = ContextCabalPackage {_modules = Map.empty, _dependencies = []}, _reqId = 0}
  run 53465 (serve api $ hoistServer api (nt s) (server constants))
