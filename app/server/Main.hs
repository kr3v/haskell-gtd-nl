{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, putMVar, readMVar, takeMVar, threadDelay)
import Control.Lens (makeLenses, (<+=), (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger (runFileLoggingT, runStdoutLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (StateT (runStateT), execStateT)
import Control.Monad.State.Lazy (evalStateT)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownSymbol)
import GTD.Configuration (GTDConfiguration (..), prepareConstants, root)
import GTD.Resolution.State (Context, emptyContext)
import GTD.Server (DefinitionRequest (..), DefinitionResponse (..), DropCacheRequest, definition, resetCache)
import GTD.Utils (ultraZoom)
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, defaultProtocol, listen, socket, socketPort, tupleToHostAddress, withSocketsDo)
import Network.Wai.Handler.Warp (defaultSettings, runSettingsSocket)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, help, helper, info, long, option, showDefault, value, (<**>))
import Servant (Header, Headers, JSON, Post, ReqBody, addHeader, (:<|>) (..), (:>))
import Servant.Server (Handler, serve)
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import System.Posix (exitImmediately, getProcessID)
import Text.Printf (printf)
import Control.Monad.IO.Class (liftIO)
import qualified GTD.Resolution.State.Caching.Cabal as CabalCache

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
      :> ReqBody '[JSON] DropCacheRequest
      :> Post '[JSON] String

api :: Proxy API
api = Proxy

type AppM = StateT (MVar ServerState) IO

type ServerStateC = MVar ServerState

nt :: ServerStateC -> AppM a -> Handler a
nt s x = liftIO (evalStateT x s)

---

h c m respP1 respP2 req = do
  s <- liftIO $ takeMVar m
  (r, s') <- flip runStateT s $ do
    rq <- reqId <+= 1
    let reqId :: String = printf "%06d" rq
    let logP = _root c </> "server.log"
    liftIO $ putStrLn $ "Got request with ID:" ++ show reqId

    r' <- ultraZoom context $ runFileLoggingT logP $ flip runReaderT c $ runExceptT $ respP1 req
    return $ respP2 reqId r'
  liftIO $ putMVar m s'
  return r

---

definitionH ::
  KnownSymbol hs =>
  GTDConfiguration ->
  MVar ServerState ->
  DefinitionRequest ->
  Handler (Headers '[Header hs String] DefinitionResponse)
definitionH c m req = do
  let defH = either (\e -> DefinitionResponse {err = Just e, srcSpan = Nothing}) id
  h c m definition (\r e -> addHeader r $ defH e) req

pingH :: MVar ServerState -> Handler String
pingH m = do
  liftIO $ modifyMVar_ m $ \s -> return s {_reqId = _reqId s + 1}
  return "pong"

dropCacheH ::
  GTDConfiguration ->
  MVar ServerState ->
  DropCacheRequest ->
  Handler String
dropCacheH c m req = do
  let defH = either id id
  h c m resetCache (\_ e -> defH e) req

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

newtype Args = Args
  { ttl :: Int
  }
  deriving (Show)

args :: Parser Args
args = Args <$> option auto (long "ttl" <> help "how long to wait before dying when idle (in seconds)" <> showDefault <> value 60)

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
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

  constants <- prepareConstants
  setCurrentDirectory (constants ^. root)
  getCurrentDirectory >>= print
  print constants

  pid <- getProcessID

  writeFile (constants ^. root </> "pid") (show pid)
  writeFile (constants ^. root </> "port") (show port)

  s0 <- flip runReaderT constants $ runStdoutLoggingT $ flip execStateT emptyServerState $ ultraZoom context CabalCache.load
  s <- newMVar s0
  _ <- forkIO $ selfKiller s (ttl as)
  runSettingsSocket defaultSettings sock $ serve api (definitionH constants s :<|> pingH s :<|> dropCacheH constants s)
