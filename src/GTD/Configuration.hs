{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Configuration where

import Control.Exception (IOException, catch)
import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Monad.Logger (LogLevel (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Version (showVersion)
import Options.Applicative (Parser, auto, help, long, option, showDefault, strOption, switch, value)
import qualified Paths_haskell_gtd
import System.Directory (createDirectoryIfMissing, getHomeDirectory, removeDirectoryRecursive)
import System.FilePath ((</>))

data Args = Args
  { _ttl :: Int,
    _dynamicMemoryUsage :: Bool,
    _logLevel :: LogLevel,
    _parserExe :: String,
    _parserArgs :: String,
    _root :: String
  }
  deriving (Show)

argsP :: Parser Args
argsP =
  Args
    <$> option auto (long "ttl" <> help "how long to wait before dying when idle (in seconds)" <> showDefault <> value 60)
    <*> switch (long "dynamic-memory-usage" <> help "whether to use dynamic memory usage" <> showDefault)
    <*> option auto (long "log-level" <> help "" <> showDefault <> value LevelInfo)
    <*> strOption (long "parser-exe" <> help "" <> showDefault <> value "./haskell-gtd-nl-parser")
    <*> strOption (long "parser-args" <> help "" <> showDefault <> value "[]")
    <*> strOption (long "root" <> help "" <> showDefault)

defaultArgs :: IO Args
defaultArgs = do
  home <- getHomeDirectory
  let root = home </> ".local" </> "share" </> "haskell-gtd-nl"
  return $ Args {_ttl = 60, _dynamicMemoryUsage = True, _logLevel = LevelInfo, _parserExe = root </> "haskell-gtd-nl-parser", _parserArgs = "[]", _root = root}

data GTDConfiguration = GTDConfiguration
  { _logs :: FilePath,
    _repos :: FilePath,
    _cache :: FilePath,
    _ccGetPath :: FilePath,
    _status :: FilePath,
    _cversion :: String,
    _args :: Args
  }
  deriving (Show)

$(makeLenses ''GTDConfiguration)

prepareConstants :: Args -> IO GTDConfiguration
prepareConstants a = do
  now <- getPOSIXTime
  let dir = _root a
  let constants =
        GTDConfiguration
          { _logs = dir </> "logs" </> show now,
            _repos = dir </> "repos",
            _cache = dir </> "cache",
            _cversion = dir </> "version",
            _status = dir </> "status",
            _ccGetPath = dir </> "cc-get.json",
            _args = a
          }
  createDirectoryIfMissing True dir
  createDirectoryIfMissing True (constants ^. logs)
  createDirectoryIfMissing True (constants ^. repos)
  createDirectoryIfMissing True (constants ^. cache)
  createDirectoryIfMissing True (constants ^. status)

  let versionCurrent = showVersion Paths_haskell_gtd.version
  versionStored <- readFile (constants ^. cversion) `catch` \(_ :: IOException) -> return ""
  when (versionStored /= versionCurrent) $ do
    putStrLn $ "Version mismatch: " ++ versionStored ++ " vs " ++ versionCurrent
    removeDirectoryRecursive (constants ^. cache)
    createDirectoryIfMissing True (constants ^. cache)
  writeFile (constants ^. cversion) versionCurrent

  return constants
