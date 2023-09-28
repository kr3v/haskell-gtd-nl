{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception.Lifted (bracket)
import Control.Lens (use)
import Control.Monad (forM_, void, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (execStateT)
import qualified GTD.Cabal.Cache as Cabal (load, store)
import qualified GTD.Cabal.Get as Cabal (changed)
import GTD.Cabal.Types (Designation (..), Package (..))
import qualified GTD.Configuration as Conf (Args (..), GTDConfiguration (..), prepareConstants)
import qualified GTD.Configuration as Configuration
import GTD.Resolution.Package (package'resolution'withDependencies'concurrently)
import GTD.Server.Definition (cabalPackage'unresolved'plusStoreInLocals)
import GTD.State (ccGet, emptyContext)
import GTD.Utils (logErrorNSS, stats, updateStatus, withLogging)
import Options.Applicative (Parser, ParserInfo, auto, execParser, fullDesc, help, helper, info, long, option, optional, strOption, (<**>))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

data Args = Args
  { dir :: String,
    sargs :: Configuration.Args,
    designation :: Maybe Designation
  }
  deriving (Show)

desP :: Parser Designation
desP =
  Designation
    <$> optional (strOption (long "designation-name" <> help ""))
    <*> option auto (long "designation-type" <> help "")

args :: Parser Args
args =
  Args
    <$> strOption (long "dir" <> help "")
    <*> Configuration.argsPJ
    <*> optional desP

opts :: ParserInfo Args
opts = info (args <**> helper) fullDesc

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  a@Args {dir = d} <- execParser opts
  print a

  let serverArgs = sargs a
  constants <- Conf.prepareConstants serverArgs
  let r = Conf._root . Conf._args $ constants
  setCurrentDirectory r
  getCurrentDirectory >>= print
  print constants

  let logP = r </> "parser.log"
      logS = r </> "status" </> "parser"
  _ <- withLogging logP logS (Configuration._logLevel . sargs $ a) $ do
    bracket (pure ()) (const $ updateStatus "") $ \_ -> do
      flip runReaderT constants $ flip execStateT emptyContext $ do
        Cabal.load
        e <- runExceptT $ do
          cPkgsU <- cabalPackage'unresolved'plusStoreInLocals d
          forM_ cPkgsU $ \p ->
            when (maybe True (_designation p ==) (designation a)) $
              void $
                package'resolution'withDependencies'concurrently p
        ccGC <- use $ ccGet . Cabal.changed
        when ccGC Cabal.store
        case e of
          Left err -> logErrorNSS d err
          Right _ -> pure ()

  stats
