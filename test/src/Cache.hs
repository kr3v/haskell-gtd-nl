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
import GTD.Configuration (Args (_powers), GTDConfiguration (..), Powers (..))
import qualified GTD.Resolution.Cache.Package as PackageCache
import qualified GTD.Resolution.Cache.Usages as UsagesCache
import qualified GTD.Server.CodeLens.LocalUsages as LocalUsages
import GTD.Server.Definition (DefinitionRequest (..), cabalPackage, definition)
import qualified GTD.Server.Definition as D
import GTD.Server.DropPackageCache (DropPackageCacheRequest (..), dropPackageCache)
import qualified GTD.Server.DropPackageCache as DC
import qualified GTD.Server.Usages as Usages
import GTD.State (emptyContext)
import GTD.Utils (removeIfExists)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Expectation, Spec, beforeAll, describe, expectationFailure, it, runIO, shouldBe)

dropCacheTest :: GTDConfiguration -> Spec
dropCacheTest consts0 = do
  let consts = consts0 {_args = (_args consts0) {_powers = (_powers . _args $ consts0) {_goToReferences_isEnabled = True, _goToReferences_lens_isEnabled = True}}}
  wd <- runIO getCurrentDirectory
  let descr = "dropCache"
      wdT = wd </> "test/integrationTestRepo/fake"
      mainF = wdT </> "executables/app/exe3/Main.hs"
      libF = wdT </> "lib1/src/Lib1.hs"
      logF = wdT </> descr ++ ".txt"
      reqDM = DefinitionRequest {D._workDir = wdT, D._file = mainF, D._word = "return"}
      reqDL = DefinitionRequest {D._workDir = wdT, D._file = libF, D._word = "return"}
      reqLUM = LocalUsages.Request wdT mainF
      reqLUL = LocalUsages.Request wdT mainF
      reqUM = Usages.Request wdT wdT mainF "return"
      reqUL = Usages.Request wdT wdT mainF "return"
  runIO $ removeIfExists logF

  let mstack f a = runFileLoggingT logF $ f $ runReaderT a consts
  runIO $ print consts

  beforeAll (mstack (`execStateT` emptyContext) Cabal.load) $
    describe descr $ do
      it "definition + dropCache" $ \st -> do
        x :: Either String Expectation <- mstack (`evalStateT` st) $ runExceptT $ execWriterT $ do
          cpkgM <-
            maybe (throwError "exe3 not found") return
              . find (\x -> (Cabal._desName . Cabal._designation $ x) == Just "exe3")
              =<< cabalPackage wdT mainF
          cpkgL <- head <$> cabalPackage wdT libF

          -- nothing is cached
          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

          -- main uses lib, so both are cached
          _ <- definition reqDM
          PackageCache.exists cpkgM >>= tell . (`shouldBe` True)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` True)

          -- main is dropped, so only main cache is dropped
          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = mainF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)

          -- lib is dropped, and main was already dropped, so nothing is cached
          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

          -- lib does not use main, so only lib is cached
          _ <- definition reqDL
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

        either (expectationFailure . show) id x
      it "local usages lens + dropCache" $ \st -> do
        x :: Either String Expectation <- mstack (`evalStateT` st) $ runExceptT $ execWriterT $ do
          cpkgM <-
            maybe (throwError "exe3 not found") return
              . find (\x -> (Cabal._desName . Cabal._designation $ x) == Just "exe3")
              =<< cabalPackage wdT mainF
          cpkgL <- head <$> cabalPackage wdT libF

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

          _ <- LocalUsages.usages reqLUM
          PackageCache.exists cpkgM >>= tell . (`shouldBe` True)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` True)

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = mainF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

          _ <- LocalUsages.usages reqLUL
          PackageCache.exists cpkgM >>= tell . (`shouldBe` True)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

        either (expectationFailure . show) id x
      it "usages + dropCache" $ \st -> do
        x :: Either String Expectation <- mstack (`evalStateT` st) $ runExceptT $ execWriterT $ do
          cpkgM <-
            maybe (throwError "exe3 not found") return
              . find (\x -> (Cabal._desName . Cabal._designation $ x) == Just "exe3")
              =<< cabalPackage wdT mainF
          cpkgL <- head <$> cabalPackage wdT libF

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

          _ <- Usages.usages reqUM
          PackageCache.exists cpkgM >>= tell . (`shouldBe` True)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` True)

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = mainF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

          _ <- Usages.usages reqUL
          PackageCache.exists cpkgM >>= tell . (`shouldBe` True)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` True)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` True)

          _ <- dropPackageCache DropCacheRequest {DC._workDir = wdT, DC._file = libF}
          PackageCache.exists cpkgM >>= tell . (`shouldBe` False)
          PackageCache.exists cpkgL >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgM >>= tell . (`shouldBe` False)
          UsagesCache.exists cpkgL >>= tell . (`shouldBe` False)

        either (expectationFailure . show) id x
