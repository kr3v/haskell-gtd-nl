{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Exception (IOException, try)
import Control.Lens (use)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (LogLevel (LevelDebug), NoLoggingT (..), runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad (forM, forM_, join)
import Control.Monad.RWS (MonadState (get), MonadWriter (..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (StateT (..), evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer (execWriterT)
import Data.Aeson (FromJSON, ToJSON, decode, eitherDecode, encode)
import Data.Aeson.Types (FromJSON (..), Parser, Value (..))
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as BS
import Data.Either (isLeft, isRight, partitionEithers)
import Data.Either.Combinators (mapLeft, mapRight)
import qualified Data.HashMap.Strict as HMap
import Data.List (find, isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (unpack)
import Distribution.Version (VersionRange)
import GHC.Generics (Generic)
import GTD.Cabal.Cache as Cabal (load, store)
import GTD.Cabal.Types (Dependency, PackageModules, PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, Version, transformPaths, transformPathsR)
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (Args (_logLevel), GTDConfiguration (..), defaultArgs, prepareConstants)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declarations (..), Exports, Imports, SourceSpan (SourceSpan, sourceSpanEndColumn, sourceSpanEndLine, sourceSpanFileName, sourceSpanStartColumn, sourceSpanStartLine), emptySourceSpan)
import GTD.Haskell.Lines (Line (..), buildMap, resolve)
import GTD.Haskell.Module (HsModule (..), HsModuleMetadata (_mPath), HsModuleP (..), emptyMetadata, parseModule)
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import qualified GTD.Resolution.Cache as PackageCache
import GTD.Resolution.Module (ModuleState (..), figureOutExports, figureOutExports'old)
import GTD.Resolution.Module.Multi (figureOutExports0)
import GTD.Server (DefinitionRequest (..), DefinitionResponse (..), DropPackageCacheRequest (..), cabalPackage, cabalPackage'contextWithLocals, cabalPackage'resolve, cabalPackage'unresolved'plusStoreInLocals, definition, dropPackageCache)
import GTD.Server.Usages (usages)
import qualified GTD.Server.Usages as Usages
import GTD.State (LocalPackagesKey, cLocalPackages, emptyContext)
import GTD.Utils (removeIfExists, storeIOExceptionToMonadError)
import System.Directory (getCurrentDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath (makeRelative, (</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe, shouldSatisfy)
import Test.Hspec.Runner (Config (configPrintCpuTime), defaultConfig, hspecWith)
import Text.Printf (printf)

haskellApplyCppHsTest :: Spec
haskellApplyCppHsTest = do
  describe "haskellApplyCppHs" $ do
    it "applies #if-related C preprocessor directives" $ do
      let srcPath = "./test/samples/haskellApplyCppHs/in.0.hs"
      let dstPath = "./test/samples/haskellApplyCppHs/out.0.hs"
      let expPath = "./test/samples/haskellApplyCppHs/exp.0.hs"

      src <- readFile srcPath
      expected <- readFile expPath

      result <- liftIO $ runExceptT $ haskellApplyCppHs srcPath src
      case result of
        Left e -> expectationFailure $ show e
        Right r -> do
          writeFile dstPath r
          r `shouldBe` expected
    it "ignores #error directive" $ do
      let srcPath = "./test/samples/haskellApplyCppHs/in.1.hs"
      let dstPath = "./test/samples/haskellApplyCppHs/out.1.hs"
      let expPath = "./test/samples/haskellApplyCppHs/exp.1.hs"

      src <- readFile srcPath
      expected <- readFile expPath

      result <- liftIO $ runExceptT $ haskellApplyCppHs srcPath src
      case result of
        Left e -> expectationFailure $ show e
        Right r -> do
          writeFile dstPath r
          r `shouldBe` expected

haskellGetIdentifiersTest :: Spec
haskellGetIdentifiersTest = do
  let descr = "haskellGetIdentifiers"
  let test n p i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ n ++ "." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- try (BS.readFile expPath) :: IO (Either IOException BS.ByteString)
        let expected :: Declarations =
              case expectedS of
                Left _ -> mempty
                Right s -> do
                  let m :: Maybe Declarations = decode s
                  fromMaybe mempty m

        result <- p srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right identifiers -> do
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  let ghcP a b = mapM (runNoLoggingT . execWriterT . GHC.identifiers) =<< runNoLoggingT (GHC.parse a b)
  let parsers = [("ghc-lib-parser", ghcP)]

  forM_ parsers $ \(n, p) -> do
    describe descr $ do
      describe n $ do
        it "parses only function declarations" $
          test n p 0
        it "parses declarations of multiple functions with shared type signature" $
          test n p 1
        it "parses classes declarations" $
          test n p 2

haskellGetExportsTest :: Spec
haskellGetExportsTest = do
  let descr = "haskellGetExportedIdentifiers"
  let test n p i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ n ++ "." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- BS.readFile expPath
        let expected :: Exports = fromJust $ decode expectedS

        result <- p srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right identifiers -> do
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  let ghcP a b = mapRight GHC.exports <$> runNoLoggingT (GHC.parse a b)
  let parsers = [("ghc-lib-parser", ghcP)]

  forM_ parsers $ \(n, p) -> do
    describe descr $ do
      describe n $ do
        it "parses a file with an explicit list of exported functions" $
          test n p 0

haskellGetImportsTest :: Spec
haskellGetImportsTest = do
  let descr = "haskellGetImportedIdentifiers"
  let test n p i = do
        let iS = show i
            srcPath = "./test/samples/" ++ descr ++ "/in." ++ iS ++ ".hs"
            dstPath = "./test/samples/" ++ descr ++ "/out." ++ n ++ "." ++ iS ++ ".json"
            expPath = "./test/samples/" ++ descr ++ "/exp." ++ iS ++ ".json"

        src <- readFile srcPath
        expectedS <- BS.readFile expPath

        let expected :: Imports = fromJust $ decode expectedS

        result <- liftIO $ p srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right identifiersM -> do
            identifiers <- identifiersM
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  let ghcP a b = mapRight (runStderrLoggingT . execWriterT . GHC.imports) <$> runNoLoggingT (GHC.parse a b)
  let parsers = [("ghc-lib-parser", ghcP)]

  forM_ parsers $ \(n, p) -> do
    describe descr $ do
      describe n $ do
        it "extracts only function imports" $
          test n p 0

figureOutExportsTest :: Spec
figureOutExportsTest = do
  consts <- runIO $ prepareConstants =<< defaultArgs

  let descr = "figureOutExports"
      root = "./test/samples/" </> descr
  tests <- runIO $ listDirectory root
  describe descr $ do
    forM_ tests $ \test -> do
      it test $ do
        let mainFileP = root </> test </> "main.hs"
        let outRP = root </> test </> "result.out.json"
        let expRP = root </> test </> "result.exp.json"
        let outMP = root </> test </> "module.out.json"
        let outS0P = root </> test </> "state0.out.json"

        expectedRS <- liftIO $ BS.readFile expRP
        let expectedR :: HsModuleP = fromJust $ decode expectedRS

        importsFs <- liftIO $ listDirectory $ root </> test
        let importsP = filter (isSuffixOf ".hs") $ filter (isPrefixOf "import.") importsFs

        importsE <- runStdoutLoggingT $ forM importsP \f -> do
          let p = root </> test </> f
          mapRight (p,) . mapLeft (printf "failed to parse %s: %s" p) <$> runExceptT (parseModule emptyMetadata {_mPath = p})
        let (errors :: [String], importedModules :: [(String, HsModule)]) = partitionEithers importsE
        liftIO $ print errors

        mainModuleE <- runStdoutLoggingT $ runExceptT $ parseModule emptyMetadata {_mPath = mainFileP}
        join $ case mainModuleE of
          Left e -> return $ expectationFailure $ printf "failed to parse %s: %s" mainFileP e
          Right mainModule -> do
            liftIO $ runStdoutLoggingT $ flip evalStateT (ModuleState HMap.empty HMap.empty) $ flip runReaderT consts $ do
              forM_ importedModules $ \(p, m) -> do
                _ <- figureOutExports'old m
                liftIO $ BS.writeFile (p ++ ".out.json") $ encode m
              st0 <- get
              liftIO $ BS.writeFile outS0P $ encode st0
              (result, _) <- figureOutExports st0 mainModule
              liftIO $ BS.writeFile outMP $ encode mainModule
              liftIO $ BS.writeFile outRP $ encode result
              return $ result `shouldBe` expectedR

definitionTests :: Spec
definitionTests = do
  da <- runIO defaultArgs
  consts <- runIO $ prepareConstants da {_logLevel = LevelDebug}
  pwd <- runIO getCurrentDirectory

  let descr = "definitions"
      wd = pwd </> "test/integrationTestRepo/fake"
      req = DefinitionRequest {workDir = wd, file = "", word = ""}
      logF = wd </> descr ++ ".txt"

  runIO $ print descr
  runIO $ printf "cwd = %s, wd = %s, logF = %s\n" pwd wd logF
  runIO $ removeIfExists logF

  let eval0 f w = runExceptT $ definition req {file = wd </> f, word = w}
      eval f w r = eval0 f w >>= (\d -> return $ d `shouldBe` r)
      mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let lib1 = "lib1/src/Lib1.hs"
      lib2 = "lib2/src/Lib2.hs"
      exe1 = "executables/app/exe1/Main.hs"
      exe2 = "executables/app/exe2/Main.hs"
      exe3 = "executables/app/exe3/Main.hs"
      ents = [lib1, lib2, exe1, exe2, exe3]
  serverState <- runIO $ mstack (`execStateT` emptyContext) $ Cabal.load >> (eval0 lib1 "return" >>= (liftIO . print)) >> Cabal.store

  let expectedPreludeReturn =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Base.hs"
            expLineNo = 862
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 5, sourceSpanEndColumn = 11, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedLib1 =
        let expFile = wd </> "lib1/src/Lib1.hs"
            expLineNo = 5
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedLib2 =
        let expFile = wd </> "lib2/src/Lib2.hs"
            expLineNo = 3
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedExe1 =
        let expFile = wd </> "executables/app/exe1/Main.hs"
            expLineNo = 6
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedExe2 =
        let expFile = wd </> "executables/app/exe2/Main.hs"
            expLineNo = 6
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedExe3 =
        let expFile = wd </> "executables/app/exe3/Main.hs"
            expLineNo = 20
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedLib2ReexportedTypeAlias =
        let expFile = wd </> "lib2/src/Lib2.hs"
            expLineNo = 6
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 6, sourceSpanEndColumn = 30, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}

      expectedMkStdGen =
        let expFile = _repos consts </> "random-1.2.1.1/src/System/Random/Internal.hs"
            expLineNo = 582
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 9, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedLensView =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Getter.hs"
            expLineNo = 244
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedLensViewOperator =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Getter.hs"
            expLineNo = 316
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedLensOverOperator =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Setter.hs"
            expLineNo = 792
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedProxy =
        let expFile = _repos consts </> "base-4.16.4.0/Data/Proxy.hs"
            expLineNo = 56
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 16, sourceSpanEndColumn = 21, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedPreludeNothing =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Maybe.hs"
            expLineNo = 29
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 19, sourceSpanEndColumn = 26, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedRunState =
        let expFile = _repos consts </> "transformers-0.5.6.2/Control/Monad/Trans/State/Lazy.hs"
            expLineNo = 109
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 9, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedPrintf =
        let expFile = _repos consts </> "base-4.16.4.0/Text/Printf.hs"
            expLineNo = 257
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 7, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedTry =
        let expFile = _repos consts </> "base-4.16.4.0/Control/Exception/Base.hs"
            expLineNo = 174
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 4, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedQMap =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Internal.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 0, sourceSpanStartLine = expLineNo, sourceSpanEndLine = 0}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedDTClockPosix =
        let expFile = _repos consts </> "time-1.12.2/lib/Data/Time/Clock/POSIX.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 0, sourceSpanStartLine = expLineNo, sourceSpanEndLine = 0}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedQMapKeys =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Internal.hs"
            expLineNo = 3347
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedGetRTSStats =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Stats.hsc"
            expLineNo = 190
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 12, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedState =
        let expFile = _repos consts </> "transformers-0.5.6.2/Control/Monad/Trans/State/Lazy.hs"
            expLineNo = 97
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 6, sourceSpanEndColumn = 11, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedDataMapStrict =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Strict.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 0, sourceSpanStartLine = expLineNo, sourceSpanEndLine = 0}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedGetter =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Type.hs"
            expLineNo = 490
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 6, sourceSpanEndColumn = 12, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedConst =
        let expFile = _repos consts </> "base-4.18.1.0/Data/Functor/Const.hs"
            expLineNo = 39
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 21, sourceSpanEndColumn = 26, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedContravariant =
        let expFile = _repos consts </> "base-4.18.1.0/Data/Functor/Contravariant.hs"
            expLineNo = 99
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 7, sourceSpanEndColumn = 20, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedSetter =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Type.hs"
            expLineNo = 292
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 6, sourceSpanEndColumn = 12, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedSettable =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Internal/Setter.hs"
            expLineNo = 32
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 57, sourceSpanEndColumn = 65, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
      expectedIdentity =
        let expFile = _repos consts </> "base-4.18.1.0/Data/Functor/Identity.hs"
            expLineNo = 57
            expSrcSpan = SourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 22, sourceSpanEndColumn = 30, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = [expSrcSpan], err = Nothing}
  let noDefErr = Right $ DefinitionResponse {srcSpan = [], err = Nothing}

  let tests =
        [(x, "from prelude - class function", "return", expectedPreludeReturn) | x <- ents]
          ++ [(x, "is visible", "lib2", expectedLib2) | x <- [lib1, lib2, exe3]]
          ++ [(x, "type alias re-export works", "Lib2_ReexportedTypeAlias", expectedLib2ReexportedTypeAlias) | x <- [lib1, lib2, exe3]]
          ++ [ (lib1, "is visible", "lib1", expectedLib1),
               (exe1, "is visible", "exe1", expectedExe1),
               (exe2, "is visible", "exe2", expectedExe2),
               (exe3, "is visible", "exe3", expectedExe3)
             ]
          ++ concat
            [ [ (x, "from prelude - function", "return", expectedPreludeReturn),
                (x, "from prelude - data ctor", "Nothing", expectedPreludeNothing),
                (x, "data type with type variable", "Proxy", expectedProxy),
                (x, "cross package re-export: class name", "State", expectedState),
                (x, "cross package re-export: module", "runState", expectedRunState),
                (x, "operator + in-package re-export: module", "^.", expectedLensViewOperator),
                (x, "operator + in-package re-export: module", "%=", expectedLensOverOperator),
                (x, "function + in-package re-export: module", "view", expectedLensView),
                (x, "function in-package re-export", "mkStdGen", expectedMkStdGen),
                (x, "qualified module import - go to module via qualifier", "Map", expectedQMap),
                (x, "qualified module import - go to module via 'original' name", "Data.Map.Strict", expectedDataMapStrict),
                (x, "ordinary module import - go to module via 'original' name", "Data.Time.Clock.POSIX", expectedDTClockPosix),
                (x, "qualified module import - go to function through qualifier", "Map.keys", expectedQMapKeys),
                (x, "", "printf", expectedPrintf),
                (x, "", "try", expectedTry),
                (x, "hsc support", "getRTSStats", expectedGetRTSStats),
                (x, "two qualified imports under the same name + type alias: does not work when no qualifier", "Getter", noDefErr),
                (x, "two qualified imports under the same name + type alias: does not work when no qualifier", "Setter", noDefErr),
                (x, "two qualified imports under the same name + type alias", "Lens.Getter", expectedGetter),
                (x, "two qualified imports under the same name + type alias", "Lens.Const", expectedConst),
                (x, "two qualified imports under the same name + type alias", "Lens.Contravariant", expectedContravariant),
                (x, "two qualified imports under the same name + type alias", "Lens.Setter", expectedSetter),
                (x, "two qualified imports under the same name + type alias", "Lens.Identity", expectedIdentity),
                (x, "two qualified imports under the same name + type alias", "Lens.Settable", expectedSettable)
              ]
              | x <- [exe3]
            ]

  describe descr $ do
    forM_ tests $ \(f, n, q, r) -> do
      it (printf "n=%s, f=%s, q=`%s`" n f q) $ do
        join $ mstack (`evalStateT` serverState) $ do
          eval f q r

data LinesSpecTestCase = LinesSpecTestCase
  { _in :: Line,
    _out :: Line
  }
  deriving (Show, Generic)

instance FromJSON LinesSpecTestCase

instance ToJSON LinesSpecTestCase

linesTest :: Spec
linesTest = do
  let descr = "lines"
  let test i = runExceptT $ do
        let iS = show i
            sampleRoot = "./test/samples/" </> descr
            srcFile = "in." ++ iS ++ ".hs"
            srcPath = sampleRoot </> srcFile
            dstPath = sampleRoot </> ("out." ++ iS ++ ".json")
            expPath = sampleRoot </> ("exp." ++ iS ++ ".json")
            testsPath = sampleRoot </> ("cases." ++ iS ++ ".json")
            dstC1Path = sampleRoot </> ("out.c1." ++ iS ++ ".hs")

        c0 <- liftIO $ readFile srcPath
        c1 <- haskellApplyCppHs srcFile c0

        liftIO $ writeFile dstC1Path c1

        expectedS <- storeIOExceptionToMonadError $ BS.readFile expPath
        let expected :: Map.Map Int Line = fromMaybe mempty $ decode expectedS

        testsS <- storeIOExceptionToMonadError $ BS.readFile testsPath
        let tests :: [LinesSpecTestCase] = fromMaybe mempty $ decode testsS

        let result = buildMap c1
        liftIO $ BS.writeFile dstPath $ encode result
        let e1 = result `shouldBe` expected

        let tst t =
              let i = _in t
                  o = _out t
               in resolve result (num i) `shouldBe` Just o
            es = tst <$> tests

        return $ foldr (<>) e1 es

  describe descr $ do
    it "???" $ do
      x <- test 0
      either (expectationFailure . show) id x

instance FromJSON Version where
  parseJSON (String t) = pure $ read $ unpack t
  parseJSON :: Value -> Parser Version

instance FromJSON VersionRange where
  parseJSON :: Value -> Parser VersionRange
  parseJSON (String t) = pure $ read $ unpack t

instance FromJSON Dependency

instance FromJSON PackageModules

instance FromJSON PackageWithUnresolvedDependencies

instance FromJSON PackageWithResolvedDependencies

cabalFullTest :: Spec
cabalFullTest = do
  da <- runIO defaultArgs
  consts <- runIO $ prepareConstants da {_logLevel = LevelDebug}
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

  describe descr $ do
    it "test repo" $ do
      test 0

dropCacheTest :: Spec
dropCacheTest = do
  da <- runIO defaultArgs
  consts <- runIO $ prepareConstants da {_logLevel = LevelDebug}
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

  describe descr $ do
    it "" $ do
      x :: Either String Expectation <- mstack (`evalStateT` st2) $ runExceptT $ execWriterT $ do
        cpkgM <-
          maybe (throwError "exe3 not found") return
            . find (\x -> (Cabal._desName . Cabal._designation $ x) == Just "exe3")
            =<< cabalPackage wdT mainF
        cpkgL <- head <$> cabalPackage wdT libF

        PackageCache.pExists cpkgM >>= tell . (`shouldBe` True)
        PackageCache.pExists cpkgL >>= tell . (`shouldBe` True)

        _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = mainF}
        PackageCache.pExists cpkgM >>= tell . (`shouldBe` False)
        PackageCache.pExists cpkgL >>= tell . (`shouldBe` True)

        _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = libF}
        PackageCache.pExists cpkgM >>= tell . (`shouldBe` False)
        PackageCache.pExists cpkgL >>= tell . (`shouldBe` False)

        _ <- definition reqL
        PackageCache.pExists cpkgM >>= tell . (`shouldBe` False)
        PackageCache.pExists cpkgL >>= tell . (`shouldBe` True)

        _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = libF}
        PackageCache.pExists cpkgM >>= tell . (`shouldBe` False)
        PackageCache.pExists cpkgL >>= tell . (`shouldBe` False)

        _ <- definition reqM
        PackageCache.pExists cpkgM >>= tell . (`shouldBe` True)
        PackageCache.pExists cpkgL >>= tell . (`shouldBe` True)

        _ <- dropPackageCache DropCacheRequest {dcDir = wdT, dcFile = libF}
        PackageCache.pExists cpkgM >>= tell . (`shouldBe` False)
        PackageCache.pExists cpkgL >>= tell . (`shouldBe` False)

      either (expectationFailure . show) id x

usagesTest :: Spec
usagesTest = do
  da <- runIO defaultArgs
  consts <- runIO $ prepareConstants da {_logLevel = LevelDebug}
  pwd <- runIO getCurrentDirectory

  let descr = "usages"
      wd = pwd </> "test/integrationTestRepo/fake"
      req = DefinitionRequest {workDir = wd, file = "", word = ""}
      logF = wd </> descr ++ ".txt"

  runIO $ print descr
  runIO $ printf "cwd = %s, wd = %s, logF = %s\n" pwd wd logF
  runIO $ removeIfExists logF

  let evalD f w = runExceptT $ definition req {workDir = wd, file = wd </> f, word = w}
      eval f w exp = do
        let rq = Usages.Request {Usages.workDir = wd, Usages.file = wd </> f, Usages.word = w}
        got <- runExceptT $ usages rq
        liftIO $ printf "%s -> %s ?= %s\n" (show rq) (show got) (show exp)
        return $ got `shouldBe` exp
      mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let lib1 = "lib1/src/Lib1.hs"
      lib2 = "lib2/src/Lib2.hs"
      exe1 = "executables/app/exe1/Main.hs"
      exe2 = "executables/app/exe2/Main.hs"
      exe3 = "executables/app/exe3/Main.hs"
      ents = [lib1, lib2, exe1, exe2, exe3]
  serverState <- runIO $ mstack (`execStateT` emptyContext) $ Cabal.load >> (evalD exe3 "return" >>= (liftIO . printf "%s %s %s -> %s\n" descr exe3 "return" . show)) >> Cabal.store

  let expectedExe3 =
        let expFile = wd </> "executables/app/exe3/Main.hs"
            expLineNo1 = 20
            expLineNo2 = 21
            expSrcSpan1 = emptySourceSpan {sourceSpanFileName = BSC8.pack expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo1, sourceSpanEndLine = expLineNo1}
            expSrcSpan2 = expSrcSpan1 {sourceSpanStartLine = expLineNo2, sourceSpanEndLine = expLineNo2}
         in Right $ Usages.Response {Usages.srcSpan = [expSrcSpan1, expSrcSpan2], Usages.err = Nothing}

  let tests = [(exe3, "local function", "exe3", expectedExe3)]

  describe descr $ do
    forM_ tests $ \(f, n, q, r) -> do
      it (printf "n=%s, f=%s, q=`%s`" n f q) $ do
        join $ mstack (`evalStateT` serverState) $ do
          eval f q r

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  c <- prepareConstants =<< defaultArgs
  removeDirectoryRecursive $ _cache c
  removeDirectoryRecursive $ _cacheUsages c

  hspecWith defaultConfig {configPrintCpuTime = False} $ do
    haskellApplyCppHsTest
    haskellGetIdentifiersTest
    haskellGetExportsTest
    haskellGetImportsTest
    linesTest
    figureOutExportsTest
    cabalFullTest
    dropCacheTest
    definitionTests
    usagesTest