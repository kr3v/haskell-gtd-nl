{-# LANGUAGE TemplateHaskell #-}

module GTD.Configuration where

import Control.Lens (makeLenses, (^.))
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))
import Control.Monad.Logger (LogLevel)

data GTDConfiguration = GTDConfiguration
  { _logs :: FilePath,
    _repos :: FilePath,
    _root :: FilePath,
    _cache :: FilePath,
    _ccGetPath :: FilePath,
    _isDynamicMemoryUsageByPackage :: Bool,
    _logLevel :: LogLevel
  }
  deriving (Show)

$(makeLenses ''GTDConfiguration)

prepareConstants :: Bool -> LogLevel -> IO GTDConfiguration
prepareConstants dm ll = do
  now <- getPOSIXTime
  homeDir <- getHomeDirectory
  let dir = homeDir </> ".local/share/haskell-gtd-extension-server-root/"
  let constants =
        GTDConfiguration
          { _root = dir,
            _logs = dir </> "logs" </> show now,
            _repos = dir </> "repos",
            _cache = dir </> "cache",
            _ccGetPath = dir </> "cc-get.json",
            _isDynamicMemoryUsageByPackage = dm,
            _logLevel = ll
          }
  createDirectoryIfMissing True (constants ^. root)
  createDirectoryIfMissing True (constants ^. logs)
  createDirectoryIfMissing True (constants ^. repos)
  createDirectoryIfMissing True (constants ^. cache)
  return constants