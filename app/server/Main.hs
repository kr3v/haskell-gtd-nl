{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (MVar, forkIO, modifyMVar, modifyMVar_, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Exception.Lifted (bracket)
import Control.Lens (makeLenses, (<+=), (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (LoggingT (..), defaultOutput, filterLogger, runFileLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), execStateT)
import Control.Monad.State.Lazy (evalStateT)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol)
import qualified GTD.Cabal.Cache as CabalCache
import GTD.Configuration (Args (..), GTDConfiguration (..), args, argsP, prepareConstants)
import GTD.State (Context, emptyContext)
import GTD.Server (CpphsRequest, CpphsResponse (..), DefinitionRequest (..), DefinitionResponse (..), DropPackageCacheRequest, cpphs, definition, dropPackageCache)
import GTD.Utils (combine, statusL, ultraZoom, updateStatus)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, defaultProtocol, listen, socket, socketPort, tupleToHostAddress, withSocketsDo)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket)
import Options.Applicative (ParserInfo, execParser, fullDesc, helper, info, (<**>))
import Servant (Header, Headers, JSON, Post, ReqBody, addHeader, (:<|>) (..), (:>), Delete)
import Servant.Server (Handler, serve)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), IOMode (..), hSetBuffering, stderr, stdout, withFile)
import System.Posix (exitImmediately, getProcessID)
import Text.Printf (printf)

data ServerState = ServerState
  { _context :: Context,
    _reqId :: Int
  }

$(makeLenses ''ServerState)

emptyServerState :: ServerState
emptyServerState = ServerState {_context = emptyContext, _reqId = 0}

-- two methods:
-- definition - to obtain a source span for a given word
-- ping - to check if the server is alive
type API =
  "definition"
    :> ReqBody '[JSON] DefinitionRequest
    :> Post '[JSON] (Headers '[Header "X-Request-ID" String] DefinitionResponse)
    :<|> "ping"
      :> Post '[JSON] String
    :<|> "dropcache"
      :> ReqBody '[JSON] DropPackageCacheRequest
      :> Post '[JSON] String
    :<|> "cpphs"
      :> ReqBody '[JSON] CpphsRequest
      :> Post '[JSON] CpphsResponse

api :: Proxy API
api = Proxy

type AppM = StateT (MVar ServerState) IO

type ServerStateC = MVar ServerState

nt :: ServerStateC -> AppM a -> Handler a
nt s x = liftIO (evalStateT x s)

---

h n c m respP1 respP2 req = do
  let logP = (_root . _args $ c) </> "server.log"
      statusP = (_root . _args $ c) </> "status" </> "server"
  liftIO $
    withFile logP AppendMode $ \h -> do
      hSetBuffering h LineBuffering
      modifyMVar m $ \s -> do
        (r, s') <- flip runStateT s $ do
          rq <- reqId <+= 1
          let reqId :: String = printf "%06d" rq
          liftIO $ putStrLn $ "Got request with ID:" ++ show reqId
          r' <-
            ultraZoom context $
              (`runLoggingT` combine (statusL statusP) (defaultOutput h)) $ do
                bracket (pure ()) (const $ updateStatus "") $ \_ -> do
                  updateStatus $ printf "preparing to execute `%s`" n
                  filterLogger (\_ l -> l >= (_logLevel . _args $ c)) $
                    flip runReaderT c $
                      runExceptT $
                        respP1 req
          return $ respP2 reqId r'
        return (s', r)

---

definitionH ::
  KnownSymbol hs =>
  GTDConfiguration ->
  MVar ServerState ->
  DefinitionRequest ->
  Handler (Headers '[Header hs String] DefinitionResponse)
definitionH c m req = do
  let defH = either (\e -> DefinitionResponse {err = Just e, srcSpan = Nothing}) id
  h "definition" c m definition (\r e -> addHeader r $ defH e) req

pingH :: MVar ServerState -> Handler String
pingH m = do
  liftIO $ modifyMVar_ m $ \s -> return s {_reqId = _reqId s + 1}
  return "pong"

dropCacheH ::
  GTDConfiguration ->
  MVar ServerState ->
  DropPackageCacheRequest ->
  Handler String
dropCacheH c m req = do
  let defH = either id id
  h "dropCache" c m dropPackageCache (\_ e -> defH e) req

runCpphsH ::
  GTDConfiguration ->
  MVar ServerState ->
  CpphsRequest ->
  Handler CpphsResponse
runCpphsH c m req = do
  let defH = either (\e -> CpphsResponse {crErr = Just e, crContent = Nothing}) id
  h "runCpphs" c m cpphs (\_ e -> defH e) req

---

selfKiller :: MVar ServerState -> Int -> IO ()
selfKiller m ttl = do
  s1 <- _reqId <$> readMVar m
  threadDelay $ ttl * 1000 * 1000
  m' <- takeMVar m
  let s2 = _reqId m'
  if s1 == s2
    then do
      putStrLn "No requests for 60 seconds, exiting _now_"
      exitImmediately ExitSuccess
    else do
      putMVar m m'
      selfKiller m ttl

opts :: ParserInfo Args
opts =
  info
    (argsP <**> helper)
    fullDesc

main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  as <- execParser opts
  print as

  let addr = SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1))
  sock <- socket AF_INET Stream defaultProtocol
  bind sock addr
  listen sock 5
  port <- socketPort sock
  printf "port=%s\n" (show port)

  constants <- prepareConstants as
  let root = _root . _args $ constants
  setCurrentDirectory root
  getCurrentDirectory >>= print
  print constants

  pid <- getProcessID

  writeFile (root </> "pid") (show pid)
  writeFile (root </> "port") (show port)

  s0 <- flip runReaderT constants $ runStdoutLoggingT $ flip execStateT emptyServerState $ ultraZoom context CabalCache.load
  s <- newMVar s0
  _ <- forkIO $ selfKiller s (_ttl as)
  runSettingsSocket defaultSettings sock $ serve api (definitionH constants s :<|> pingH s :<|> dropCacheH constants s :<|> runCpphsH constants s)
