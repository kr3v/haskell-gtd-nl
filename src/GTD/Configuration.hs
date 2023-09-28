{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Configuration where

import Control.Concurrent (QSem, newQSem)
import Control.Exception (IOException, catch)
import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..))
import Data.Aeson (eitherDecodeStrict)
import qualified Data.ByteString.Char8 as BS
import Data.Version (showVersion)
import GTD.Utils.OS.Memory (availableMemory, totalMemory)
import Options.Applicative (Parser, auto, eitherReader, help, long, option, showDefault, strOption, switch, value)
import qualified Paths_haskell_gtd
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.Info (os)
import Text.Printf (printf)

data Args = Args
  { _ttl :: Int,
    _dynamicMemoryUsage :: Bool,
    _logLevel :: LogLevel,
    _parserExe :: String,
    _parserArgs :: [String],
    _root :: String
  }
  deriving (Show)

parseJson :: String -> Either String [String]
parseJson = eitherDecodeStrict . BS.pack

argsP :: IO (Parser Args)
argsP = do
  home <- getHomeDirectory
  let root = home </> ".local" </> "share" </> "haskell-gtd-nl"
  let cabalBin = home </> ".cabal" </> "bin" </> "haskell-gtd-nl-parser"
  return $
    Args
      <$> option auto (long "ttl" <> help "how long to wait before dying when idle (in seconds)" <> showDefault <> value 60)
      <*> switch (long "dynamic-memory-usage" <> help "whether to use dynamic memory usage" <> showDefault)
      <*> option auto (long "log-level" <> help "" <> showDefault <> value LevelInfo)
      <*> strOption (long "parser-exe" <> help "" <> showDefault <> value cabalBin)
      <*> option (eitherReader parseJson) (long "parser-args" <> help "" <> showDefault <> value [])
      <*> strOption (long "root" <> help "" <> showDefault <> value root)

defaultArgs :: IO Args
defaultArgs = do
  home <- getHomeDirectory
  let root =
        if os == "darwin"
          then home </> "Library" </> "Application Support" </> "Code" </> "haskell-gtd-nl"
          else home </> ".local" </> "share" </> "haskell-gtd-nl"
  let cabalBin = home </> ".cabal" </> "bin" </> "haskell-gtd-nl-parser"
  return $ Args {_ttl = 60, _dynamicMemoryUsage = True, _logLevel = LevelInfo, _parserExe = cabalBin, _parserArgs = [], _root = root}

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
