{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative (Alternative (empty), Applicative (liftA2))
import Control.Exception (evaluate)
import Control.Lens (At (at), use)
import Control.Lens.Prism (_Just)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.State (MonadTrans (lift), StateT (..), evalStateT, execStateT, forM)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer (MonadIO (liftIO), execWriterT, forM_, join, runWriterT)
import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, encode, genericToJSON)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription (emptyGenericPackageDescription, emptyPackageDescription)
import GHC.RTS.Flags (ProfFlags (descrSelector))
import GTD.Cabal
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (_repos), prepareConstants)
import GTD.Haskell.AST (Declarations (..), Exports, Imports, haskellGetExports, haskellGetIdentifiers, haskellGetImports, haskellParse)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declaration (Declaration, _declName, _declSrcOrig), Identifier (..), SourceSpan (SourceSpan, sourceSpanEndColumn, sourceSpanEndLine, sourceSpanFileName, sourceSpanStartColumn, sourceSpanStartLine), emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule)
import GTD.Haskell.Package
import qualified GTD.Haskell.Package as Package
import qualified GTD.Haskell.Resolution as Resolution
import qualified GTD.Haskell.Module as HsModule
import GTD.Server (DefinitionRequest (..), DefinitionResponse (..), definition, noDefintionFoundError, noDefintionFoundErrorE, contextStoreGetCache, contextFetchGetCache)
import GTD.Utils (logDebugNSS, ultraZoom)
import Language.Haskell.Exts (Module (..), SrcSpan (..), SrcSpanInfo (..))
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe, shouldNotBe, shouldSatisfy)
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

      result <- liftIO $ runExceptT $ haskellApplyCppHs srcPath src
      case result of
        Left err -> expectationFailure $ show err
        Right result -> do
          writeFile dstPath result
          result `shouldBe` expected
    it "ignores #error directive" $ do
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
        let expected :: Declarations = fromJust $ decode expectedS

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
    it "parses classes declarations" $
      test 2

haskellGetExportsSpec :: Spec
haskellGetExportsSpec = do
  let descr = "haskellGetExportedIdentifiers"
  let test i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- BS.readFile expPath

        let expected :: Exports = fromJust $ decode expectedS

        let result = haskellParse srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right m -> do
            (isImplicitExportAll, identifiers) <- runWriterT $ runStderrLoggingT $ haskellGetExports m
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  describe descr $ do
    it "parses a file with an explicit list of exported functions" $
      test 0

instance FromJSON Imports

instance ToJSON Imports

haskellGetImportsSpec :: Spec
haskellGetImportsSpec = do
  let descr = "haskellGetImportedIdentifiers"
  let test i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- BS.readFile expPath

        let expected :: Imports = fromJust $ decode expectedS

        let result = haskellParse srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right m -> do
            identifiers <- execWriterT $ runStderrLoggingT $ haskellGetImports m
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected
  describe descr $ do
    it "extracts only function imports" $
      test 0

emptyCabalPackage :: Cabal.Package
emptyCabalPackage = Cabal.Package "" emptyPackageDescription

cmGen mn0 dn0 mnE dnE fnE =
  let d0 = Declaration emptySourceSpan mn0 dn0
      lE = emptySourceSpan {sourceSpanFileName = fnE}
      dE = d0 {_declSrcOrig = lE, _declName = dnE}
      cmE = Map.fromList [(mnE, HsModuleP {HsModule._exports = Declarations {_decls = Map.fromList [(dnE, dE)], _dataTypes = Map.empty}})]
   in (d0, dE, cmE)

nothingWasExpected d0 d1 = expectationFailure $ printf "expected Nothing, got %s (== (%s) => %s)" (show d0) (show d1) (show (d0 == d1))

-- enrichTryModuleSpec :: Spec
-- enrichTryModuleSpec = do
--   describe "enrichTryModule" do
--     it "returns nothing in case of a missing module name" $
--       property $ \moduleName1 moduleName2 declName fileName ->
--         moduleName1 /= moduleName2 && fileName /= "" ==> do
--           let (d0, dE, cmE) = cmGen moduleName1 declName moduleName2 declName fileName
--           result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryModule d0 cmE
--           forM_ result (nothingWasExpected d0)
--     it "returns nothing if there's a matching module without a matching declaration" $
--       property $ \moduleName declName1 declName2 fileName ->
--         declName1 /= declName2 && fileName /= "" ==> do
--           let (d0, dE, cmE) = cmGen moduleName declName1 moduleName declName2 fileName
--           result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryModule d0 cmE
--           forM_ result (nothingWasExpected d0)
--     it "actually enriches in case of matched module and declaration" $
--       property $ \moduleName declName fileName ->
--         fileName /= "" ==> do
--           let (d0, dE, cmE) = cmGen moduleName declName moduleName declName fileName
--           result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryModule d0 cmE
--           case result of
--             Nothing -> expectationFailure $ printf "expected Just %s, got Nothing" (show d0)
--             Just d1 -> do
--               _declSrcOrig d1 `shouldBe` _declSrcOrig dE
--               _declSrcUsage d1 `shouldNotBe` _declSrcUsage dE

-- enrichTryPackageSpec :: Spec
-- enrichTryPackageSpec = do
--   describe "enrichTryPackage" do
--     it "returns nothing in case of an exported module from a missing package" $
--       property \moduleName declName fileName packageName1 packageName2 ->
--         fileName /= "" && packageName1 /= packageName2 ==> do
--           let (d0, dE, cmE) = cmGen moduleName declName moduleName declName fileName
--               pmE = Map.fromList [(packageName1, cmE)]
--               cp = emptyCabalPackage {_cabalPackageName = packageName2, _cabalPackageExportedModules = Map.singleton moduleName ModuleName.main}
--           result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryPackage d0 pmE cp
--           forM_ result (nothingWasExpected d0)
--     it "returns nothing in case of a non-exported module name" $
--       property \moduleName1 moduleName2 declName fileName packageName ->
--         fileName /= "" && moduleName1 /= moduleName2 ==> do
--           let (d0, dE, cmE) = cmGen moduleName1 declName moduleName1 declName fileName
--               pmE = Map.fromList [(packageName, cmE)]
--               cp = emptyCabalPackage {_cabalPackageName = packageName, _cabalPackageExportedModules = Map.singleton moduleName2 ModuleName.main}
--           result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryPackage d0 pmE cp
--           forM_ result (nothingWasExpected d0)
--     it "actually enriches in case of an exported module and matching package name" $
--       property \moduleName declName fileName packageName ->
--         fileName /= "" ==> do
--           let (d0, dE, cmE) = cmGen moduleName declName moduleName declName fileName
--               pmE = Map.fromList [(packageName, cmE)]
--               cp = emptyCabalPackage {_cabalPackageName = packageName, _cabalPackageExportedModules = Map.singleton moduleName ModuleName.main}
--           result <- liftIO $ runStderrLoggingT $ runMaybeT $ enrichTryPackage d0 pmE cp
--           case result of
--             Nothing -> expectationFailure $ printf "expected Just %s, got Nothing" (show d0)
--             Just d1 -> do
--               _declSrcOrig d1 `shouldBe` _declSrcOrig dE
--               _declSrcUsage d1 `shouldNotBe` _declSrcUsage dE

definitionsSpec :: Spec
definitionsSpec = do
  consts <- runIO prepareConstants

  let descr = "definitions"

  let workDir = "./test/integrationTestRepo/sc-ea-hs"
  let file = workDir </> "app/game/Main.hs"
  let req = DefinitionRequest {workDir = "./test/integrationTestRepo/sc-ea-hs", file = file, word = ""}

  let eval0 w = runExceptT (definition req {word = w})
  let eval w r = eval0 w >>= (\x -> return $ x `shouldBe` r)
  let mstack f s a = runFileLoggingT (workDir </> descr ++ ".txt") $ flip f s $ runReaderT a consts

  let expectedPlayIO =
        let expFile = _repos consts </> "gloss-1.13.2.2/Graphics/Gloss/Interface/IO/Game.hs"
            expLineNo = 20
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 7, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedMkStdGen =
        let expFile = _repos consts </> "random-1.2.1.1/src/System/Random/Internal.hs"
            expLineNo = 582
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 9, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedLensView =
        let expFile = _repos consts </> "lens-5.2.2/src/Control/Lens/Getter.hs"
            expLineNo = 244
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedLensOverOperator =
        let expFile = _repos consts </> "lens-5.2.2/src/Control/Lens/Setter.hs"
            expLineNo = 792
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedDisplay =
        let expFile = _repos consts </> "gloss-1.13.2.2/Graphics/Gloss/Data/Display.hs"
            expLineNo = 7
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 6, sourceSpanEndColumn = 13, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedInWindow =
        let expFile = _repos consts </> "gloss-1.13.2.2/Graphics/Gloss/Data/Display.hs"
            expLineNo = 9
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 11, sourceSpanEndColumn = 19, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedProxy =
        let expFile = _repos consts </> "base-4.16.4.0/Data/Proxy.hs"
            expLineNo = 56
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 16, sourceSpanEndColumn = 21, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let noDefErr = Left "No definition found"

  let st0 = emptyContext
  st1 <- runIO $ mstack execStateT st0 contextFetchGetCache
  (a, serverState) <- runIO $ mstack runStateT st1 $ eval0 "playIO"
  runIO $ print a
  runIO $ mstack execStateT serverState contextStoreGetCache

  describe descr $ do
    it "directly exported regular function" $ do
      join $ mstack evalStateT serverState $ eval "playIO" expectedPlayIO
    it "sequential execution does not fail" $ do
      join $ mstack evalStateT serverState $ do
        eval "playIO" expectedPlayIO
        eval "playIO" expectedPlayIO
        eval "playIO" expectedPlayIO
    it "re-exported regular function" $ do
      join $ mstack evalStateT serverState $ do
        eval "mkStdGen" expectedMkStdGen

    it "re-exported throughout packages (?) class name" $ do
      join $ mstack evalStateT serverState $ do
        eval "State" noDefErr
    it "cross-package module re-export" $ do
      join $ mstack evalStateT serverState $ do
        eval "runState" noDefErr
    it "(re-export)? data name" $ do
      join $ mstack evalStateT serverState $ do
        eval "Picture" noDefErr

    it "data type name" $ do
      join $ mstack evalStateT serverState $ do
        eval "Display" expectedDisplay
    it "data constructor" $ do
      join $ mstack evalStateT serverState $ do
        eval "InWindow" expectedInWindow

    it "data type with type variable" $ do
      join $ mstack evalStateT serverState $ do
        eval "Proxy" expectedProxy

    it "in-package module re-export + operator form 1" $ do
      join $ mstack evalStateT serverState $ do
        eval "^." noDefErr
        eval "%=" expectedLensOverOperator
    it "in-package module re-export + operator form 2" $ do
      join $ mstack evalStateT serverState $ do
        eval "(^.)" noDefErr
        eval "(%=)" noDefErr
    it "in-package module re-export + function" $ do
      join $ mstack evalStateT serverState $ do
        eval "view" expectedLensView

-- parsePackageSpec :: Spec
-- parsePackageSpec = do
--   let descr = "parsePackage"
--   describe descr $
--     it "for each dependency in the integration repo, it parses all its modules" $ do
--       let expPath = "./test/samples/" ++ descr ++ "/exp.0.json"
--       let dstPath = "./test/samples/" ++ descr ++ "/out.0.json"

--       let tWorkDir = "./test/integrationTestRepo/sc-ea-hs"
--       let mFile = tWorkDir </> "app/game/Main.hs"

--       consts <- prepareConstants
--       result <- flip evalStateT emptyServerState $ runFileLoggingT (tWorkDir </> descr ++ ".txt") $ flip runReaderT consts $ do
--         let req = DefinitionRequest {workDir = tWorkDir, file = mFile, word = ""}
--         x1 <- runExceptT $ definition req {word = "playIO"}
--         pkgM <- use $ context . ccFindAt . at tWorkDir
--         case pkgM of
--           Nothing -> error "package not found"
--           Just pkgR -> do
--             deps <- Cabal._dependencies <$> Cabal.full pkgR
--             depsF <- mapM Cabal.full deps
--             r <- forM depsF $ \dep -> do
--               modules <- Map.elems <$> ultraZoom context (Resolution.modules1 dep)
--               liftIO $ print (Cabal.nameF dep)
--               liftIO $ printf "\tmods=%s\n" (show $ HsModule._name . _m <$> modules)
--               return (Cabal.nameF dep, HsModule._name . _m <$> modules)
--             return $ Map.fromList r

--       expectedS <- BS.readFile expPath
--       let expected :: Map.Map PackageNameS [ModuleNameS] = fromJust $ decode expectedS
--       BS.writeFile dstPath $ encode result

--       -- explicitly verify that there are non-exported modules that were parsed
--       -- yet be aware of 'failed' modules, as some Cabal-exported modules might be missing from the returned value
--       result `shouldBe` expected

-- updateExportsSpec :: Spec
-- updateExportsSpec = do
--   let descr = "updateExports"
--   describe descr $
--     it "1" $ do
--       let dstPath m = "./test/samples/" ++ descr ++ "/" ++ m ++ ".json"

--       let tWorkDir = "./test/integrationTestRepo/sc-ea-hs"
--       let mFile = tWorkDir </> "app/game/Main.hs"

--       -- let lift3 = lift . lift . lift

--       consts <- prepareConstants
--       flip evalStateT emptyServerState $ runFileLoggingT (tWorkDir </> descr ++ ".txt") $ flip runReaderT consts $ do
--         let req = DefinitionRequest {workDir = tWorkDir, file = mFile, word = ""}
--         x1 <- runExceptT $ definition req {word = "playIO"}

--         pkg <- use $ context . ccpmodules . at "haskell-src-exts" . _Just
--         liftIO $ print $ Map.keys pkg

--         let m1 = "Language.Haskell.Exts.Extension"
--         liftIO $ BS.writeFile (dstPath m1) $ encode $ _exports $ fromJust $ Map.lookup m1 pkg
--         let m2 = "Language.Haskell.Exts"
--         liftIO $ BS.writeFile (dstPath m2) $ encode $ _exports $ fromJust $ Map.lookup m2 pkg
--         let m3 = "Language.Haskell.Exts.Syntax"
--         liftIO $ BS.writeFile (dstPath m3) $ encode $ _exports $ fromJust $ Map.lookup m3 pkg

--       True `shouldBe` True

---

integrationTestsSpec :: Spec
integrationTestsSpec = do
  definitionsSpec
  -- parsePackageSpec

-- updateExportsSpec

main :: IO ()
main = hspecWith defaultConfig {configPrintCpuTime = False} $ do
  haskellApplyCppHsSpec
  haskellGetIdentifiersSpec
  haskellGetExportsSpec
  haskellGetImportsSpec
  integrationTestsSpec
