{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens (use, (^.))
import Control.Monad.Except (runExceptT, void, when)
import Control.Monad.Logger (runFileLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (execStateT)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (prepareConstants, GTDConfiguration(..), root)
import GTD.Resolution.State (ccGet, emptyContext)
import qualified GTD.Resolution.State.Caching.Cabal as CabalCache
import GTD.Utils (stats)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, long, strOption, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import GTD.Server (package'resolution'withDependencies'concurrently)
import System.FilePath ((</>))

newtype Args = Args
  { dir :: String
  }
  deriving (Show)

args :: Parser Args
args = Args <$> strOption (long "dir" <> help "how long to wait before dying when idle (in seconds)")

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    fullDesc

main :: IO ()
main = do
  constants <- prepareConstants
  setCurrentDirectory (constants ^. root)
  getCurrentDirectory >>= print
  print constants

  let logP = _root constants </> "package.log"

  Args {dir = d} <- execParser opts

  void $ runExceptT $ flip runReaderT constants $ runFileLoggingT logP $ flip execStateT emptyContext $ do
    CabalCache.load
    pkg <- CabalCache.findAt d
    _ <- package'resolution'withDependencies'concurrently pkg
    ccGC <- use $ ccGet . Cabal.changed
    when ccGC CabalCache.store
  stats
