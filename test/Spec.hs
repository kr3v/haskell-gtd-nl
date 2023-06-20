{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (evaluate)
import Control.Monad.Logger
import Control.Monad.Writer (MonadIO (liftIO), execWriterT, runWriterT)
import Data.Aeson (decode, defaultOptions, encode, genericToJSON)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromJust)
import GTD.Haskell (Declaration, haskellApplyCppHs, haskellGetExportedIdentifiers, haskellGetIdentifiers, haskellGetImportedIdentifiers, haskellParse)
import System.Directory (getCurrentDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Test.Hspec.Runner (Config (configPrintCpuTime), defaultConfig, hspecWith)
import Text.Printf (printf)

haskellApplyCppHsSpec :: Spec
haskellApplyCppHsSpec = do
  describe "haskellApplyCppHs" $ do
    it "applies #if-related C preprocessor directives" $ do
      let srcPath = "./test/samples/haskellApplyCppHs/in.0.hs"
      let dstPath = "./test/samples/haskellApplyCppHs/out.0.hs"
      let expPath = "./test/samples/haskellApplyCppHs/exp.0.hs"

      src <- readFile srcPath
      expected <- readFile expPath

      result <- haskellApplyCppHs srcPath src

      writeFile dstPath result

      printf "src:\n```\n%s\n```\n\n" src
      printf "result:\n```\n%s\n```\n\n" result
      printf "expected:\n```\n%s\n```\n\n" expected
      result `shouldBe` expected

-- TODO: `shouldBe` invocation should not rely on the order of declarations, as the fact that
--       declarations are ordered would not be required later
haskellGetIdentifiersSpec :: Spec
haskellGetIdentifiersSpec = do
  let descr = "haskellGetIdentifiers"
  let test i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- BS.readFile expPath

        let expected :: [Declaration] = fromJust $ decode expectedS

        let result = haskellParse srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right m -> do
            identifiers <- liftIO $ runStderrLoggingT $ execWriterT $ haskellGetIdentifiers m
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  describe descr $ do
    it "parses only function declarations" $
      test 0
    it "parses declarations of multiple functions with shared type signature" $
      test 1

haskellGetExportedIdentifiersSpec :: Spec
haskellGetExportedIdentifiersSpec = do
  let descr = "haskellGetExportedIdentifiers"
  let test i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- BS.readFile expPath

        let expected :: [Declaration] = fromJust $ decode expectedS

        let result = haskellParse srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right m -> do
            (isImplicitExportAll, identifiers) <- runWriterT $ runStderrLoggingT $ haskellGetExportedIdentifiers m
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  describe descr $ do
    it "parses a file with an explicit list of exported functions" $
      test 0

haskellGetImportedIdentifiersSpec :: Spec
haskellGetImportedIdentifiersSpec = do
  let descr = "haskellGetImportedIdentifiers"
  let test i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- BS.readFile expPath

        let expected :: [Declaration] = fromJust $ decode expectedS

        let result = haskellParse srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right m -> do
            identifiers <- execWriterT $ runStderrLoggingT $ haskellGetImportedIdentifiers m
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected
  describe descr $ do
    it "extracts only function imports" $
      test 0

main :: IO ()
main = hspecWith defaultConfig {configPrintCpuTime = False} $ do
  haskellApplyCppHsSpec
  haskellGetIdentifiersSpec
  haskellGetExportedIdentifiersSpec
  haskellGetImportedIdentifiersSpec
