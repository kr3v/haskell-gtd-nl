{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GTD.Configuration where

import Control.Concurrent (QSem, newQSem)
import Control.Exception (IOException, catch)
import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..), MonadLoggerIO)
import Control.Monad.RWS (MonadReader)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Aeson (FromJSON, ToJSON, Value, eitherDecodeStrict)
import Data.Aeson.Types (FromJSON (..), ToJSON (..), Value (..))
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Version (showVersion)
import GHC.Generics (Generic)
import GTD.Utils.OS.Memory (availableMemory, totalMemory)
import Options.Applicative (Parser, auto, eitherReader, help, long, option, showDefault, strOption, switch, value)
import qualified Paths_haskell_gtd
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Info (os)
import Text.Printf (printf)

instance FromJSON LogLevel where
  parseJSON :: Value -> JSON.Parser LogLevel
  parseJSON (String t) = pure $ read $ T.unpack t

instance ToJSON LogLevel where
  toJSON :: LogLevel -> Value
  toJSON = toJSON . show

data Powers = Powers
  { _goToReferences_isEnabled :: Bool,
    _goToReferences_limit :: Int
  }
  deriving (Show, Generic)

instance FromJSON Powers

instance ToJSON Powers

data Args = Args
  { _ttl :: Int,
    _dynamicMemoryUsage :: Bool,
    _logLevel :: LogLevel,
    _parserExe :: String,
    _parserArgs :: [String],
    _root :: String,
    _powers :: Powers
  }
  deriving (Show, Generic)

$(makeLenses ''Args)

instance FromJSON Args

instance ToJSON Args

parseJson :: FromJSON a => String -> Either String a
parseJson = eitherDecodeStrict . BS.pack

defaultRoot :: String -> String
defaultRoot home =
  if os == "darwin"
    then home </> "Library" </> "Application Support" </> "Code" </> "haskell-gtd-nl"
    else home </> ".local" </> "share" </> "haskell-gtd-nl"

powersP :: Parser Powers
powersP =
  Powers
    <$> switch (long "go-to-references" <> help "whether to support 'Go to References'" <> showDefault)
    <*> option auto (long "usages-limit" <> help "" <> showDefault <> value 256)

argsP :: IO (Parser Args)
argsP = do
  home <- getHomeDirectory
  let root = defaultRoot home
  let cabalBin = home </> ".cabal" </> "bin" </> "haskell-gtd-nl-parser"
  return $
    Args
      <$> option auto (long "ttl" <> help "how long to wait before dying when idle (in seconds)" <> showDefault <> value 60)
      <*> switch (long "dynamic-memory-usage" <> help "whether to use dynamic memory usage" <> showDefault)
      <*> option auto (long "log-level" <> help "" <> showDefault <> value LevelInfo)
      <*> strOption (long "parser-exe" <> help "" <> showDefault <> value cabalBin)
      <*> option (eitherReader parseJson) (long "parser-args" <> help "" <> showDefault <> value [])
      <*> strOption (long "root" <> help "" <> showDefault <> value root)
      <*> powersP

-- Args parser from JSON string
argsPJ :: Parser Args
argsPJ = option (eitherReader parseJson) (long "args" <> help "" <> showDefault)

defaultArgs :: IO Args
defaultArgs = do
  home <- getHomeDirectory
  let root = defaultRoot home
  let cabalBin = home </> ".cabal" </> "bin" </> "haskell-gtd-nl-parser"
  return $
    Args
      { _ttl = 60,
        _dynamicMemoryUsage = True,
        _logLevel = LevelInfo,
        _parserExe = cabalBin,
        _parserArgs = [],
        _root = root,
        _powers =
          Powers
            { _goToReferences_isEnabled = False,
              _goToReferences_limit = 256
            }
      }

data GTDConfiguration = GTDConfiguration
  { _repos :: FilePath,
    _cache :: FilePath,
    _cacheUsages :: FilePath,
    _ccGetPath :: FilePath,
    _status :: FilePath,
    _cversion :: String,
    _cabalGetSemaphore :: QSem,
    _args :: Args
  }

instance Show GTDConfiguration where
  show :: GTDConfiguration -> String
  show c = "GTDConfiguration { _repos = " ++ _repos c ++ ", _cache = " ++ _cache c ++ ", _ccGetPath = " ++ _ccGetPath c ++ ", _status = " ++ _status c ++ ", _cversion = " ++ _cversion c ++ " }"

$(makeLenses ''GTDConfiguration)

prepareConstants :: Args -> IO GTDConfiguration
prepareConstants a = do
  -- `cabal get` uses ~150 MiB of memory; using `200` as a safety margin
  let cabalGetMemoryUsage = 200
  total <- (`div` 8) . (`div` cabalGetMemoryUsage) . (`div` (1024 * 1024)) <$> totalMemory
  usable <- (`div` 4) . (`div` cabalGetMemoryUsage) . (`div` (1024 * 1024)) <$> availableMemory
  let cgKeys = max (1024 `div` cabalGetMemoryUsage) (min total usable)
  -- allow at least some concurrency for `cabal get`, but limited by total/free memory
  cgSemaphore <- newQSem cgKeys
  printf "max through totals = %s, max through usable = %s, result = %s\n" (show total) (show usable) (show cgKeys)

  let dir = _root a
  let constants =
        GTDConfiguration
          { _repos = dir </> "repos",
            _cache = dir </> "cache" </> "packages",
            _cacheUsages = dir </> "cache" </> "usages",
            _cversion = dir </> "version",
            _status = dir </> "status",
            _ccGetPath = dir </> "cc-get.json",
            _cabalGetSemaphore = cgSemaphore,
            _args = a
          }
  createDirectoryIfMissing True dir
  createDirectoryIfMissing True (constants ^. repos)
  createDirectoryIfMissing True (constants ^. cache)
  createDirectoryIfMissing True (constants ^. cacheUsages)
  createDirectoryIfMissing True (constants ^. status)

  let versionCurrent = showVersion Paths_haskell_gtd.version
  versionStored <- readFile (constants ^. cversion) `catch` \(_ :: IOException) -> return ""
  when (versionStored /= versionCurrent) $ do
    putStrLn $ "Version mismatch: " ++ versionStored ++ " vs " ++ versionCurrent
    removeDirectoryRecursive (constants ^. cache)
    createDirectoryIfMissing True (constants ^. cache)
  writeFile (constants ^. cversion) versionCurrent

  return constants

type MS0 m = (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m)