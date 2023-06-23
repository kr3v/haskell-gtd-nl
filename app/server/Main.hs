{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import GHC.TypeLits
import qualified GHC.TypeLits
import GTD.Cabal (cabalDeps, cabalPackageName, cabalPackagePath, cabalRead)
import GTD.Configuration (GTDConfiguration (_logs), prepareConstants, root)
import GTD.Haskell (ContextCabalPackage (..), HsModule (..), Declaration (..), Identifier (Identifier), SourceSpan (..), dependencies, parseModule, parsePackages)
import GTD.Server (DefinitionRequest, DefinitionResponse (..), ServerState (..), definition, reqId)
import GTD.Utils (deduplicateBy, ultraZoom)
import Network.Socket (AddrInfo (addrAddress), Family (AF_INET), SockAddr (SockAddrInet), Socket (..), SocketType (Stream), bind, defaultProtocol, listen, openSocket, socket, socketPort, tupleToHostAddress, withSocketsDo)
import Network.Wai.Handler.Warp (defaultSettings, run, runSettingsSocket)
import Numeric (showHex)
import Servant (Get, Handler, Header, Headers, JSON, Post, Proxy (..), ReqBody, addHeader, serve, type (:<|>) (..), type (:>))
import Servant.Server
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, getHomeDirectory, listDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, (</>))
import System.Posix (getProcessID)
import System.Posix.Signals (Signal (..), addSignal, awaitSignal, emptySignalSet, getPendingSignals, inSignalSet, keyboardSignal, signalProcess)
import System.Process (getPid)
import System.Process.Internals (ProcessHandle (waitpidLock))
import System.Random.Stateful (StdGen, mkStdGen, randomIO)
import Text.Printf (printf)

-- two methods:
-- definition - to obtain a source span for a given word
-- ping - to check if the server is alive

type API =
  "definition"
    :> ReqBody '[JSON] DefinitionRequest
    :> Post '[JSON] (Headers '[Header "X-Request-ID" String] DefinitionResponse)
    :<|> "ping"
      :> Get '[JSON] String

api :: Proxy API
api = Proxy

type AppM = StateT (MVar ServerState) IO

type ServerStateC = MVar ServerState

nt :: ServerStateC -> AppM a -> Handler a
nt s x = liftIO (evalStateT x s)

flipTuple :: (a, b) -> (b, a)
flipTuple (a, b) = (b, a)

definitionH ::
  KnownSymbol h =>
  GTDConfiguration ->
  MVar ServerState ->
  DefinitionRequest ->
  Handler (Headers '[Header h String] DefinitionResponse)
definitionH c m req = do
  s <- liftIO $ takeMVar m
  (r, s') <- flip runStateT s $ do
    rq <- reqId <+= 1
    let reqId = printf "%06d" rq
    let log = _logs c </> (reqId ++ ".log")
    liftIO $ putStrLn $ "Got request with ID:" ++ show reqId

    r' <- runReaderT (runFileLoggingT log (runExceptT $ definition req)) c
    case r' of
      Left e -> return $ addHeader reqId DefinitionResponse {srcSpan = Nothing, err = Just e}
      Right r -> return $ addHeader reqId r
  liftIO $ putMVar m s'
  return r

pingH :: Handler String
pingH = return "pong"

main :: IO ()
main = withSocketsDo $ do
  let addr = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)) -- SockAddrUnix String
  sock <- socket AF_INET Stream defaultProtocol
  bind sock addr
  listen sock 5
  port <- socketPort sock
  printf "port=%s\n" (show port)

  constants <- prepareConstants
  setCurrentDirectory (constants ^. root)
  getCurrentDirectory >>= print
  print constants

  pid <- getProcessID

  writeFile (constants ^. root </> "pid") (show pid)
  writeFile (constants ^. root </> "port") (show port)

  s <- newMVar $ ServerState {_context = ContextCabalPackage {_modules = Map.empty, _dependencies = []}, _reqId = 0}
  runSettingsSocket defaultSettings sock $ serve api (definitionH constants s :<|> pingH)
