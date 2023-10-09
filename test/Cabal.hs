{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Cabal where

import Control.Lens (use)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, eitherDecode, encode)
import Data.Aeson.Types (FromJSON (..), Parser, Value (..))
import qualified Data.ByteString.Lazy as BS
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Text (unpack)
import Distribution.Version (VersionRange)
import GTD.Cabal.Cache as Cabal (load)
import GTD.Cabal.Types (Dependency, PackageModules, PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, Version, transformPaths, transformPathsR)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Server.Definition (cabalPackage'contextWithLocals, cabalPackage'resolve, cabalPackage'unresolved'plusStoreInLocals)
import GTD.State (LocalPackagesKey, cLocalPackages, emptyContext)
import GTD.Utils (removeIfExists)
import System.Directory (getCurrentDirectory)
import System.FilePath (makeRelative, (</>))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)
import Text.Printf (printf)

instance FromJSON Version where
  parseJSON :: Value -> Parser Version
  parseJSON (String t) = pure $ read $ unpack t

instance FromJSON VersionRange where
  parseJSON :: Value -> Parser VersionRange
  parseJSON (String t) = pure $ read $ unpack t

instance FromJSON Dependency

instance FromJSON PackageModules

instance FromJSON PackageWithUnresolvedDependencies

instance FromJSON PackageWithResolvedDependencies

cabalFullTest :: GTDConfiguration -> Spec
cabalFullTest consts = do
  wd <- runIO getCurrentDirectory

  let descr = "cabalFull"
      wdT = wd </> "test/integrationTestRepo/fake"
      logF = wdT </> descr ++ ".txt"
  runIO $ removeIfExists logF

  let mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let st0 = emptyContext
  st1 <- runIO $ mstack (`execStateT` st0) Cabal.load

  let test0 i = do
        let iS = show i
            dstUPath = "test/samples" </> descr </> ("out.unresolved." ++ iS ++ ".json")
            dstLPath = "test/samples" </> descr </> ("out.locals." ++ iS ++ ".json")
            dstRPath = "test/samples" </> descr </> ("out.resolved." ++ iS ++ ".json")
            expUPath = "test/samples" </> descr </> ("exp.unresolved." ++ iS ++ ".json")
            expLPath = "test/samples" </> descr </> ("exp.locals." ++ iS ++ ".json")
            expRPath = "test/samples" </> descr </> ("exp.resolved." ++ iS ++ ".json")

        expectedUS <- liftIO $ BS.readFile expUPath
        expectedLS <- liftIO $ BS.readFile expLPath
        expectedRS <- liftIO $ BS.readFile expRPath
        expectedR :: [PackageWithResolvedDependencies] <- either (throwError . show) return $ eitherDecode expectedRS
        expectedU :: [PackageWithUnresolvedDependencies] <- either (throwError . show) return $ eitherDecode expectedUS
        expectedL :: [LocalPackagesKey] <- either (throwError . show) return $ eitherDecode expectedLS

        mstack (`evalStateT` st1) $ do
          result <- runExceptT $ cabalPackage'unresolved'plusStoreInLocals wdT
          case result of
            Left e -> return $ expectationFailure $ printf "failed to parse %s: %s" wdT e
            Right u -> do
              let t p
                    | wd `isPrefixOf` p = makeRelative wd p
                    | _repos consts `isPrefixOf` p = ".repos" </> makeRelative (_repos consts) p
                    | otherwise = p
                  uT = transformPaths t <$> u

              liftIO $ BS.writeFile dstUPath $ encode uT
              let d1 = uT `shouldBe` expectedU

              cabalPackage'contextWithLocals u
              l0 <- use cLocalPackages
              let l1 = concatMap (\((k1, k2), vs) -> (\k3 -> (k1, k2, k3)) <$> Map.keys vs) (Map.assocs l0)
              liftIO $ BS.writeFile dstLPath $ encode l1
              let d2 = l1 `shouldBe` expectedL

              r <- cabalPackage'resolve u
              let rT = transformPathsR t <$> r
              liftIO $ BS.writeFile dstRPath $ encode rT
              let d3 = rT `shouldBe` expectedR

              return $ d1 <> d2 <> d3

  let test i = do
        x <- runExceptT $ test0 i
        case x of
          Left e -> expectationFailure e
          Right r -> r

  describe descr $ it "test repo" $ do
    test 0
