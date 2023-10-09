{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Cabal (cabalFullTest)
import Cache (dropCacheTest)
import Control.Monad.Logger (LogLevel (LevelDebug), runStderrLoggingT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Definitions (definitionTests)
import GTD.Configuration (Args (..), defaultArgs, prepareConstants, resetCache)
import Haskell (haskellApplyCppHsTest, haskellGetExportsTest, haskellGetIdentifierUsagesTest, haskellGetIdentifiersTest, haskellGetImportsTest, linesTest)
import Resolution (figureOutExportsTest)
import System.Directory (makeAbsolute)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Test.Hspec.Runner (Config (configPrintCpuTime), defaultConfig, hspecWith)
import Usages (usagesTest)
import LocalUsages (localUsagesTest)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  r <- makeAbsolute "test/root"
  a <- defaultArgs
  c <- prepareConstants a {_logLevel = LevelDebug, _root = r}
  runStderrLoggingT $ runReaderT resetCache c

  hspecWith defaultConfig {configPrintCpuTime = False} $ do
    -- haskellApplyCppHsTest c
    -- haskellGetIdentifiersTest c
    -- haskellGetExportsTest c
    -- haskellGetImportsTest c
    -- haskellGetIdentifierUsagesTest c
    -- linesTest c

    -- figureOutExportsTest c
    -- cabalFullTest c

    -- dropCacheTest c
    localUsagesTest c
    -- definitionTests c
    -- usagesTest c