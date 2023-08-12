{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens (use, (^.))
import Control.Monad (forM_)
import Control.Monad.Except (runExceptT, when)
import Control.Monad.Logger (runFileLoggingT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (execStateT)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..), prepareConstants, root)
import GTD.Resolution.State (ccGet, emptyContext)
import qualified GTD.Resolution.State.Caching.Cabal as CabalCache
import GTD.Server (package'resolution'withDependencies'concurrently)
import GTD.Utils (logErrorNSS, stats)
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, help, helper, info, long, strOption, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

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
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  constants <- prepareConstants
  setCurrentDirectory (constants ^. root)
  getCurrentDirectory >>= print
  print constants

  Args {dir = d} <- execParser opts

  runFileLoggingT (_root constants </> "package.log") $ do
    e <- runExceptT $ flip runReaderT constants $ flip execStateT emptyContext $ do
      CabalCache.load
      pkg <- CabalCache.findAt d
      forM_ pkg $ \p -> package'resolution'withDependencies'concurrently p
      ccGC <- use $ ccGet . Cabal.changed
      when ccGC CabalCache.store
    case e of
      Left err -> logErrorNSS d err
      Right _ -> pure ()

  stats
