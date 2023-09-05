{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Lifted (bracket)
import Control.Lens (use)
import Control.Monad (forM_)
import Control.Monad.Except (MonadIO (..), runExceptT, when)
import Control.Monad.Logger (LogLevel (LevelInfo), LoggingT (..), defaultOutput, filterLogger)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (execStateT)
import qualified GTD.Cabal.Cache as Cabal (load, store)
import qualified GTD.Cabal.Get as Cabal (changed)
import qualified GTD.Configuration as Conf (Args (..), GTDConfiguration (..), defaultArgs, prepareConstants)
import GTD.Resolution.State (ccGet, emptyContext)
import GTD.Server (cabalPackage'contextWithLocals, cabalPackage'unresolved, package'resolution'withDependencies'concurrently)
import GTD.Utils (combine, logErrorNSS, stats, statusL, updateStatus)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, help, helper, info, long, option, showDefault, strOption, value, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), IOMode (..), hSetBuffering, stderr, stdout, withFile)

data Args = Args
  { dir :: String,
    logLevel :: LogLevel
  }
  deriving (Show)

args :: Parser Args
args =
  Args
    <$> strOption (long "dir" <> help "how long to wait before dying when idle (in seconds)")
    <*> option auto (long "log-level" <> help "" <> showDefault <> value LevelInfo)

opts :: ParserInfo Args
opts =
  info
    (args <**> helper)
    fullDesc

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  a@Args {dir = d, logLevel = ll} <- execParser opts
  print a

  constants <- Conf.prepareConstants =<< Conf.defaultArgs
  let r = Conf._root . Conf._args $ constants
  setCurrentDirectory r
  getCurrentDirectory >>= print
  print constants

  let logP = r </> "parser.log"
      logS = r </> "status" </> "parser"
  withFile logP AppendMode $ \h ->
    (`runLoggingT` combine (statusL logS) (defaultOutput h)) $
      filterLogger (\_ l -> l >= ll) $ do
        bracket (pure ()) (const $ updateStatus "") $ \_ -> do
          liftIO $ hSetBuffering h LineBuffering
          flip runReaderT constants $ flip execStateT emptyContext $ do
            Cabal.load
            e <- runExceptT $ do
              cPkgsU <- cabalPackage'unresolved d
              cabalPackage'contextWithLocals cPkgsU
              forM_ cPkgsU $ \p -> package'resolution'withDependencies'concurrently p
            ccGC <- use $ ccGet . Cabal.changed
            when ccGC Cabal.store
            case e of
              Left err -> logErrorNSS d err
              Right _ -> pure ()

  stats
