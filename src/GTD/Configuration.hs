{-# LANGUAGE TemplateHaskell #-}

module GTD.Configuration where

import Control.Lens (makeLenses, (^.))
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, getHomeDirectory)
import System.FilePath ((</>))

data GTDConfiguration = GTDConfiguration
  { _logs :: FilePath,
    _repos :: FilePath,
    _root :: FilePath,
    _ccGetPath :: FilePath
  }
  deriving (Show)

$(makeLenses ''GTDConfiguration)

prepareConstants :: IO GTDConfiguration
prepareConstants = do
  now <- getPOSIXTime
  homeDir <- getHomeDirectory
  let dir = homeDir </> ".local/share/haskell-gtd-extension-server-root/"
  let constants = GTDConfiguration {_root = dir, _logs = dir </> "logs" </> show now, _repos = dir </> "repos", _ccGetPath = dir </> "cc-get.json"}
  createDirectoryIfMissing True (constants ^. root)
  createDirectoryIfMissing True (constants ^. logs)
  createDirectoryIfMissing True (constants ^. repos)
  return constants