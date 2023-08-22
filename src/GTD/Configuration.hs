{-# LANGUAGE TemplateHaskell #-}

module GTD.Configuration where

import Control.Lens (makeLenses, (^.))
import Control.Monad.Logger (LogLevel (..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Options.Applicative (Parser, auto, help, long, option, showDefault, switch, value)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data Args = Args
  { _ttl :: Int,
    _dynamicMemoryUsage :: Bool,
    _logLevel :: LogLevel,
    _packageExe :: String,
    _root :: String
  }
  deriving (Show)

argsP :: Parser Args
argsP =
  Args
    <$> option auto (long "ttl" <> help "how long to wait before dying when idle (in seconds)" <> showDefault <> value 60)
    <*> switch (long "dynamic-memory-usage" <> help "whether to use dynamic memory usage" <> showDefault)
    <*> option auto (long "log-level" <> help "" <> showDefault <> value LevelInfo)
    <*> option auto (long "package-exe" <> help "" <> showDefault <> value "./haskell-gtd-package")
    <*> option auto (long "root" <> help "" <> showDefault)

defaultArgs :: IO Args
defaultArgs = do
  home <- getHomeDirectory
  let root = home </> ".local" </> "share" </> "haskell-gtd-extension-server-root"
  return $ Args {_ttl = 60, _dynamicMemoryUsage = True, _logLevel = LevelInfo, _packageExe = root </> "haskell-gtd-package", _root = root}

data GTDConfiguration = GTDConfiguration
  { _logs :: FilePath,
    _repos :: FilePath,
    _cache :: FilePath,
    _ccGetPath :: FilePath,
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
            _ccGetPath = dir </> "cc-get.json",
            _args = a
          }
  createDirectoryIfMissing True dir
  createDirectoryIfMissing True (constants ^. logs)
  createDirectoryIfMissing True (constants ^. repos)
  createDirectoryIfMissing True (constants ^. cache)
  return constants