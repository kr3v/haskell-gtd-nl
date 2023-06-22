{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (Alternative (empty), Applicative (liftA2))
import Control.Exception (evaluate)
import Control.Lens
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.State (MonadTrans (lift), StateT (..), evalStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer (MonadIO (liftIO), execWriterT, forM_, join, runWriterT)
import Data.Aeson (decode, defaultOptions, encode, genericToJSON)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription (emptyGenericPackageDescription, emptyPackageDescription)
import GHC.RTS.Flags (ProfFlags (descrSelector))
import GTD.Cabal (CabalPackage (..))
import GTD.Configuration
import GTD.Haskell (ContextModule (..), Declaration (..), Identifier (Identifier), SourceSpan (..), dependencies, emptyContextModule, emptySourceSpan, enrichTryModule, enrichTryPackage, haskellApplyCppHs, haskellGetExportedIdentifiers, haskellGetIdentifiers, haskellGetImportedIdentifiers, haskellParse, parsePackage, parsePackages)
import GTD.Server (DefinitionRequest (..), DefinitionResponse (..), context, definition, emptyServerState, noDefintionFoundError, noDefintionFoundErrorE)
import GTD.Utils (logDebugNSS, ultraZoom)
import Language.Haskell.Exts (Module (..), SrcSpan (..), SrcSpanInfo (..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.Hspec.Runner (Config (configPrintCpuTime), defaultConfig, hspecWith)
import Test.QuickCheck (Testable (..), forAll, (==>))
import Text.Printf (printf)
import qualified Distribution.ModuleName as Cabal
import qualified Data.Set as Set

haskellApplyCppHsSpec :: Spec
haskellApplyCppHsSpec = do
  describe "haskellApplyCppHs" $ do
    it "applies #if-related C preprocessor directives" $ do
      let srcPath = "./test/samples/haskellApplyCppHs/in.0.hs"
      let dstPath = "./test/samples/haskellApplyCppHs/out.0.hs"
      let expPath = "./test/samples/haskellApplyCppHs/exp.0.hs"

      src <- readFile srcPath
      expected <- readFile expPath

      result <- liftIO $ runExceptT $ haskellApplyCppHs srcPath src
      case result of
        Left err -> expectationFailure $ show err
        Right result -> do
          writeFile dstPath result
          result `shouldBe` expected
    it "fails on #error directive" $ do
      let srcPath = "./test/samples/haskellApplyCppHs/in.1.hs"
      let dstPath = "./test/samples/haskellApplyCppHs/out.1.hs"
      let expPath = "./test/samples/haskellApplyCppHs/exp.1.hs"

      src <- readFile srcPath
      expected <- readFile expPath

      result <- liftIO $ runExceptT $ haskellApplyCppHs srcPath src
      case result of
        Left err -> expectationFailure $ show err
        Right result -> do
          writeFile dstPath result
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

emptyCabalPackage :: CabalPackage
emptyCabalPackage = CabalPackage "" "" emptyPackageDescription [] Map.empty

cmGen mn0 dn0 mnE dnE fnE =
  let d0 = Declaration emptySourceSpan emptySourceSpan mn0 dn0
      lE = emptySourceSpan {sourceSpanFileName = fnE}
      dE = d0 {_declSrcOrig = lE, _declSrcUsage = lE, _declName = dnE}
      cmE = Map.fromList [(mnE, emptyContextModule {_exports = Map.fromList [(Identifier dnE, dE)]})]
   in (d0, dE, cmE)

nothingWasExpected d0 d1 = expectationFailure $ printf "expected Nothing, got %s (== (%s) => %s)" (show d0) (show d1) (show (d0 == d1))

enrichTryModuleSpec :: Spec
enrichTryModuleSpec = do
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

enrichTryPackageSpec :: Spec
enrichTryPackageSpec = do
  describe "enrichTryPackage" do
    it "returns nothing in case of an exported module from a missing package" $
      property \moduleName declName fileName packageName1 packageName2 ->
        fileName /= "" && packageName1 /= packageName2 ==> do
          let (d0, dE, cmE) = cmGen moduleName declName moduleName declName fileName
              pmE = Map.fromList [(packageName1, cmE)]
              cp = emptyCabalPackage {_cabalPackageName = packageName2, _cabalPackageExportedModules = Map.singleton moduleName ModuleName.main}
          result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryPackage d0 pmE cp
          forM_ result (nothingWasExpected d0)
    it "returns nothing in case of a non-exported module name" $
      property \moduleName1 moduleName2 declName fileName packageName ->
        fileName /= "" && moduleName1 /= moduleName2 ==> do
          let (d0, dE, cmE) = cmGen moduleName1 declName moduleName1 declName fileName
              pmE = Map.fromList [(packageName, cmE)]
              cp = emptyCabalPackage {_cabalPackageName = packageName, _cabalPackageExportedModules = Map.singleton moduleName2 ModuleName.main}
          result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryPackage d0 pmE cp
          forM_ result (nothingWasExpected d0)
    it "actually enriches in case of an exported module and matching package name" $
      property \moduleName declName fileName packageName ->
        fileName /= "" ==> do
          let (d0, dE, cmE) = cmGen moduleName declName moduleName declName fileName
              pmE = Map.fromList [(packageName, cmE)]
              cp = emptyCabalPackage {_cabalPackageName = packageName, _cabalPackageExportedModules = Map.singleton moduleName ModuleName.main}
          result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryPackage d0 pmE cp
          case result of
            Nothing -> expectationFailure $ printf "expected Just %s, got Nothing" (show d0)
            Just d1 -> do
              _declSrcOrig d1 `shouldBe` _declSrcOrig dE
              _declSrcUsage d1 `shouldNotBe` _declSrcUsage dE

definitionsSpec :: Spec
definitionsSpec = do
  describe "definitions" $ do
    it "1" $ do
      consts <- prepareConstants

      let workDir = "./test/integrationTestRepo/sc-ea-hs"
      let file = workDir </> "app/game/Main.hs"
      let req = DefinitionRequest {workDir = "./test/integrationTestRepo/sc-ea-hs", file = file, word = ""}

      let expectedPlayIO =
            let expFile = _repos consts </> "gloss-1.13.2.2/./Graphics/Gloss/Interface/IO/Game.hs"
                expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartLine = 20, sourceSpanStartColumn = 1, sourceSpanEndLine = 20, sourceSpanEndColumn = 7}
             in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}

      flip evalStateT emptyServerState $ runFileLoggingT (workDir </> "log1.txt") $ flip runReaderT consts $ do
        noDefErr <- noDefintionFoundErrorE

        t11 <- liftIO getCurrentTime
        x1 <- runExceptT $ definition req {word = "playIO"}
        t12 <- liftIO getCurrentTime

        t21 <- liftIO getCurrentTime
        x2 <- runExceptT $ definition req {word = "playIO"}
        t22 <- liftIO getCurrentTime

        t31 <- liftIO getCurrentTime
        x3 <- runExceptT $ definition req {word = "playIO"}
        t32 <- liftIO getCurrentTime

        logDebugNSS "definitionsSpec" $ show x1
        logDebugNSS "definitionsSpec" $ show x2
        logDebugNSS "definitionsSpec" $ show x3

        let d1 = diffUTCTime t12 t11
        let d2 = diffUTCTime t22 t21
        let d3 = diffUTCTime t32 t31
        logDebugNSS "definitionsSpec" $ show (d1, d2, d3)

        lift $ lift $ lift $ do
          x1 `shouldBe` expectedPlayIO
          x2 `shouldBe` expectedPlayIO
          x3 `shouldBe` expectedPlayIO
          -- more than one second
          d1 `shouldSatisfy` (> 1)
          d2 `shouldSatisfy` (> 1)
          d3 `shouldSatisfy` (> 1)
          -- second time should be noticeably faster (not a proper way of testing this, but anyway)
          d1 / d2 `shouldSatisfy` (> 1.4)
          -- should not differ much
          d2 / d3 `shouldSatisfy` liftA2 (&&) (< 1.1) (> (1 / 1.1))

        -- re-exported regular function
        x4 <- runExceptT $ definition req {word = "mkStdGen"}
        logDebugNSS "definitionsSpec" $ show x4
        lift $ lift $ lift $ x4 `shouldBe` noDefErr

        -- class name
        x5 <- runExceptT $ definition req {word = "State"}
        logDebugNSS "definitionsSpec" $ show x5
        lift $ lift $ lift $ x5 `shouldBe` noDefErr

        -- data member
        x6 <- runExceptT $ definition req {word = "runExceptT"}
        logDebugNSS "definitionsSpec" $ show x6
        lift $ lift $ lift $ x6 `shouldBe` noDefErr

parsePackageSpec :: Spec
parsePackageSpec = do
  describe "parse package" $
    it "does the thing" $ do
      let workDir = "./test/integrationTestRepo/sc-ea-hs"
      let file = workDir </> "app/game/Main.hs"

      consts <- prepareConstants
      let req = DefinitionRequest {workDir = "./test/integrationTestRepo/sc-ea-hs", file = file, word = ""}
      flip evalStateT emptyServerState $ runFileLoggingT (workDir </> "log1.txt") $ flip runReaderT consts $ do
        x1 <- runExceptT $ definition req {word = "playIO"}
        deps <- use (context . dependencies)
        let pkg = head deps
        modules1 <- ultraZoom context (parsePackage pkg)
        liftIO $ print (_cmoduleName <$> modules1)

      True `shouldBe` True

---

integrationTestsSpec :: Spec
integrationTestsSpec = do
  definitionsSpec

main :: IO ()
main = hspecWith defaultConfig {configPrintCpuTime = False} $ do
  -- parsePackageSpec
  haskellApplyCppHsSpec
  -- haskellGetIdentifiersSpec
  -- haskellGetExportedIdentifiersSpec
  -- haskellGetImportedIdentifiersSpec
  -- enrichTryModuleSpec
  -- enrichTryPackageSpec
  -- integrationTestsSpec
