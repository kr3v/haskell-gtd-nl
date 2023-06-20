{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent (MVar, modifyMVar_, newMVar, putMVar, takeMVar)
import Control.Concurrent.MVar (modifyMVar)
import Control.Lens (Zoom (zoom), makeLenses, use, (%=), (.=), (^.))
import Control.Lens.Combinators (view)
import Control.Monad.Cont (MonadIO (..))
import Control.Monad.State (StateT (runStateT), get, gets)
import Control.Monad.State.Lazy (evalStateT, execStateT, lift)
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Data (Proxy (..))
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GHC.MVar (MVar (MVar))
import GTD.Cabal (cabalDeps, cabalPackageName, cabalRead, cabalPackagePath)
import GTD.Haskell (ContextCabalPackage (..), ContextModule (c2_identifiers), Declaration (declName, declSrcOrig), Identifier (Identifier), SourceSpan (..), dependencies, parseModule, parsePackages)
import Network.Wai.Handler.Warp (run)
import Servant (HasServer (..), Server)
import Servant.API (JSON, Post, ReqBody, type (:>))
import Servant.Server (Application, Handler, hoistServer, serve)
import System.Directory (listDirectory, setCurrentDirectory)
import System.FilePath (takeExtension, (</>))
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

type API = "definition" :> ReqBody '[JSON] DefinitionRequest :> Post '[JSON] DefinitionResponse

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

definition :: DefinitionRequest -> StateT ServerState IO DefinitionResponse
definition req@(DefinitionRequest {workDir = workDir, file = file, word = word}) = do
  lift $ setCurrentDirectory workDir
  files <- lift $ listDirectory workDir
  let cabalFiles = filter (\x -> takeExtension x == ".cabal") files
  lift $ print cabalFiles
  case length cabalFiles of
    0 -> error "No cabal file found"
    1 -> do
      let cabalFile = workDir </> head cabalFiles
      lift $ print $ "Found cabal file: " ++ cabalFile
      pkg <- lift $ cabalRead $ cabalFile
      deps' <- lift $ cabalDeps pkg
      let deps = Map.elems $ Map.fromList $ (\d -> (d ^. cabalPackagePath, d)) <$> deps'

      lift $ printf "deps list = %s\n" (show $ view cabalPackageName <$> deps)

      context . dependencies .= deps
      zoom context parsePackages

      defs <- zoom context (parseModule True file)
      case defs of
        Left e -> error $ "No definition found" ++ show e
        Right m -> case Identifier word `Map.lookup` c2_identifiers m of
          Nothing -> error $ "No definition found"
          Just d -> return DefinitionResponse {srcSpan = declSrcOrig d}
    _ -> error "Multiple cabal files found"

server :: ServerT API AppM
server req = do
  m <- get
  liftIO $ print "Got request"
  let mut :: ServerState -> IO (ServerState, DefinitionResponse)
      mut x = (\(a, b) -> (b, a)) <$> runStateT (definition req) x
  liftIO $ modifyMVar m mut

main :: IO ()
main = do
  print "Starting server..."
  s <- newMVar $ ServerState {_context = ContextCabalPackage {_modules = Map.empty, _dependencies = []}}
  run 8080 (serve api $ hoistServer api (nt s) server)