{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (evaluate)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Writer (MonadIO (liftIO), execWriterT, forM_, join, runWriterT)
import Data.Aeson (decode, defaultOptions, encode, genericToJSON)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import GTD.Haskell (ContextModule (..), Declaration (..), Identifier (Identifier), SourceSpan (..), emptySourceSpan, enrichTryModule, haskellApplyCppHs, haskellGetExportedIdentifiers, haskellGetIdentifiers, haskellGetImportedIdentifiers, haskellParse)
import Language.Haskell.Exts (Module (..), SrcSpan (..), SrcSpanInfo (..))
import System.Directory (getCurrentDirectory)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldNotBe)
import Test.Hspec.Runner (Config (configPrintCpuTime), defaultConfig, hspecWith)
import Test.QuickCheck (Testable (..), forAll, (==>))
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

emptySrcSpan :: SrcSpan
emptySrcSpan = SrcSpan {srcSpanFilename = "", srcSpanStartLine = 0, srcSpanStartColumn = 0, srcSpanEndLine = 0, srcSpanEndColumn = 0}

emptySrcSpanInfo :: SrcSpanInfo
emptySrcSpanInfo = SrcSpanInfo {srcInfoSpan = emptySrcSpan, srcInfoPoints = []}

emptyHaskellModule :: Module SrcSpanInfo
emptyHaskellModule = Module emptySrcSpanInfo Nothing [] [] []

emptyContextModule :: ContextModule
emptyContextModule = ContextModule {_cmodule = emptyHaskellModule, _exports = Map.empty, _identifiers = Map.empty}

enrichTryModuleSpec :: Spec
enrichTryModuleSpec = do
  let nothingWasExpected d0 d1 = expectationFailure $ printf "expected Nothing, got %s (== (%s) => %s)" (show d0) (show d1) (show (d0 == d1))
  let cmGen mn0 dn0 mnE dnE fnE =
        let d0 = Declaration emptySourceSpan emptySourceSpan mn0 dn0
            lE = emptySourceSpan {sourceSpanFileName = fnE}
            dE = d0 {_declSrcOrig = lE, _declSrcUsage = lE, _declName = dnE}
            cmE = Map.fromList [(mnE, emptyContextModule {_exports = Map.fromList [(Identifier dnE, dE)]})]
         in (d0, dE, cmE)

  describe "enrichTryModule" do
    it "returns nothing in case of a missing module name" $
      property $ \moduleName1 moduleName2 declName fileName ->
        moduleName1 /= moduleName2 && fileName /= "" ==> do
          let (d0, dE, cmE) = cmGen moduleName1 declName moduleName2 declName fileName
          result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryModule d0 cmE
          forM_ result (nothingWasExpected d0)
    it "returns nothing if there's a matching module without a matching declaration" $
      property $ \moduleName declName1 declName2 fileName ->
        declName1 /= declName2 && fileName /= "" ==> do
          let (d0, dE, cmE) = cmGen moduleName declName1 moduleName declName2 fileName
          result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryModule d0 cmE
          forM_ result (nothingWasExpected d0)
    it "actually enriches in case of matched module and declaration" $
      property $ \moduleName declName fileName ->
        fileName /= "" ==> do
          let (d0, dE, cmE) = cmGen moduleName declName moduleName declName fileName
          result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryModule d0 cmE
          case result of
            Nothing -> expectationFailure $ printf "expected Just %s, got Nothing" (show d0)
            Just d1 -> do
              _declSrcOrig d1 `shouldBe` _declSrcOrig dE
              _declSrcUsage d1 `shouldNotBe` _declSrcUsage dE

main :: IO ()
main = hspecWith defaultConfig {configPrintCpuTime = False} $ do
  haskellApplyCppHsSpec
  haskellGetIdentifiersSpec
  haskellGetExportedIdentifiersSpec
  haskellGetImportedIdentifiersSpec
  enrichTryModuleSpec
