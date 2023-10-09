{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cache where

import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.RWS (MonadWriter (..))
import Control.Monad.State (StateT (..), evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer (execWriterT)
import Data.List (find)
import GTD.Cabal.Cache as Cabal (load)
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (GTDConfiguration (..))
import qualified GTD.Resolution.Cache.Package as PackageCache
import GTD.Server.Definition (DefinitionRequest (..), cabalPackage, definition)
import GTD.Server.DropPackageCache (DropPackageCacheRequest (..), dropPackageCache)
import GTD.State (emptyContext)
import GTD.Utils (removeIfExists)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe)

dropCacheTest :: GTDConfiguration -> Spec
dropCacheTest consts = do
  wd <- runIO getCurrentDirectory

  let descr = "dropCache"
      wdT = wd </> "test/integrationTestRepo/fake"
      mainF = wdT </> "executables/app/exe3/Main.hs"
      libF = wdT </> "lib1/src/Lib1.hs"
      logF = wdT </> descr ++ ".txt"
      reqM = DefinitionRequest {workDir = wdT, file = mainF, word = "return"}
      reqL = DefinitionRequest {workDir = wdT, file = libF, word = "return"}
  runIO $ removeIfExists logF

  let mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let st0 = emptyContext
  st1 <- runIO $ mstack (`execStateT` st0) Cabal.load
  (_, st2) <- runIO $ mstack (`runStateT` st1) $ runExceptT $ definition reqM

  describe descr $ it "" $ do
    x :: Either String Expectation <- mstack (`evalStateT` st2) $ runExceptT $ execWriterT $ do
      cpkgM <-
        maybe (throwError "exe3 not found") return
          . find (\x -> (Cabal._desName . Cabal._designation $ x) == Just "exe3")
          =<< cabalPackage wdT mainF
      cpkgL <- head <$> cabalPackage wdT libF

      PackageCache.exists cpkgM >>= tell . (`shouldBe` True)
      PackageCache.exists cpkgL >>= tell . (`shouldBe` True)

      _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = mainF}
      PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
      PackageCache.exists cpkgL >>= tell . (`shouldBe` True)

      _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = libF}
      PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
      PackageCache.exists cpkgL >>= tell . (`shouldBe` False)

      _ <- definition reqL
      PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
      PackageCache.exists cpkgL >>= tell . (`shouldBe` True)

      _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = libF}
      PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
      PackageCache.exists cpkgL >>= tell . (`shouldBe` False)

      _ <- definition reqM
      PackageCache.exists cpkgM >>= tell . (`shouldBe` True)
      PackageCache.exists cpkgL >>= tell . (`shouldBe` True)

      _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = libF}
      PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
      PackageCache.exists cpkgL >>= tell . (`shouldBe` False)

    either (expectationFailure . show) id x
