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
import Control.Monad.Logger (LogLevel (LevelDebug), NoLoggingT (..), runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.RWS (MonadIO (liftIO), MonadState (get), MonadTrans (..), MonadWriter (..), forM, forM_, join)
import Control.Monad.State (StateT (..), evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer (execWriterT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Types (FromJSON (..), Parser, ToJSON (..), Value (..))
import qualified Data.ByteString.Lazy as BS
import Data.Either (partitionEithers)
import Data.Either.Combinators (mapLeft, mapRight)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (pack, unpack)
import Distribution.Version (VersionRange)
import GHC.Generics (Generic)
import GTD.Cabal.Cache as Cabal (load, store)
import GTD.Cabal.Types (Dependency, PackageModules, PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, Version, transformPaths, transformPathsR)
import GTD.Configuration (Args (_logLevel), GTDConfiguration (..), defaultArgs, prepareConstants)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declarations (..), Exports, Imports, SourceSpan (SourceSpan, sourceSpanEndColumn, sourceSpanEndLine, sourceSpanFileName, sourceSpanStartColumn, sourceSpanStartLine))
import GTD.Haskell.Lines (Line (..), buildMap, resolve)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import qualified GTD.Resolution.Cache as PackageCache
import GTD.Resolution.Module (figureOutExports, figureOutExports0)
import GTD.Resolution.State (LocalPackagesKey, cLocalPackages, emptyContext)
import GTD.Server (DefinitionRequest (..), DefinitionResponse (..), DropPackageCacheRequest (..), cabalPackage, cabalPackage'contextWithLocals, cabalPackage'resolve, cabalPackage'unresolved, definition, dropPackageCache)
import GTD.Utils (removeIfExists, storeIOExceptionToMonadError)
import System.Directory (getCurrentDirectory, listDirectory, removeDirectoryRecursive)
import System.FilePath (makeRelative, (</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, runIO, shouldBe)
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

        result <- liftIO $ p srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right identifiersM -> do
            identifiers <- identifiersM
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  let ghcP a b = mapRight (runStderrLoggingT . flip execStateT mempty . GHC.exports) <$> runNoLoggingT (GHC.parse a b)
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
        let outDP = root </> test </> "debug.out.json"

        expectedRS <- liftIO $ BS.readFile expRP
        let expectedR :: HsModuleP = fromJust $ decode expectedRS

        importsFs <- liftIO $ listDirectory $ root </> test
        let importsP = filter (isSuffixOf ".hs") $ filter (isPrefixOf "import.") importsFs

        importsE <- runStdoutLoggingT $ forM importsP \f -> do
          let p = root </> test </> f
          mapRight (p,) . mapLeft (printf "failed to parse %s: %s" p) <$> runExceptT (parseModule emptyHsModule {_path = p})
        let (errors :: [String], importedModules :: [(String, HsModule)]) = partitionEithers importsE
        liftIO $ print errors

        mainModuleE <- runStdoutLoggingT $ runExceptT $ parseModule emptyHsModule {_path = mainFileP}
        join $ case mainModuleE of
          Left e -> return $ expectationFailure $ printf "failed to parse %s: %s" mainFileP e
          Right mainModule -> do
            liftIO $ runStdoutLoggingT $ flip evalStateT Map.empty $ flip runReaderT consts $ do
              forM_ importedModules $ \(p, m) -> do
                _ <- figureOutExports m
                liftIO $ BS.writeFile (p ++ ".out.json") $ encode m
              st0 <- get
              liftIO $ BS.writeFile outS0P $ encode st0
              (result, _, debugInfo) <- figureOutExports0 st0 mainModule
              liftIO $ BS.writeFile outDP $ encode debugInfo
              liftIO $ BS.writeFile outMP $ encode mainModule
              liftIO $ BS.writeFile outRP $ encode result
              return $ result `shouldBe` expectedR

definitionsTest :: Spec
definitionsTest = do
  da <- runIO defaultArgs
  consts <- runIO $ prepareConstants da {_logLevel = LevelDebug}
  pwd <- runIO getCurrentDirectory

  let descr = "definitions"
      workDir = pwd </> "test/integrationTestRepo/sc-ea-hs"
      file = workDir </> "app/game/Main.hs"
      req = DefinitionRequest {workDir = workDir, file = file, word = ""}
      logF = workDir </> descr ++ ".txt"

  runIO $ removeIfExists logF

  let eval0 w = runExceptT $ definition req {word = w}
      eval w r = eval0 w >>= (\d -> return $ d `shouldBe` r)
      mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let st0 = emptyContext
  st1 <- runIO $ mstack (`execStateT` st0) Cabal.load
  (a, serverState) <- runIO $ mstack (`runStateT` st1) $ eval0 "playIO"
  runIO $ print a
  -- TODO: check the warning
  _ <- runIO $ mstack (`execStateT` serverState) Cabal.store

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
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Getter.hs"
            expLineNo = 244
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedLensViewOperator =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Getter.hs"
            expLineNo = 316
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedLensOverOperator =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Setter.hs"
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
  let expectedPreludeNothing =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Maybe.hs"
            expLineNo = 29
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 19, sourceSpanEndColumn = 26, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedPicture =
        let expFile = _repos consts </> "gloss-rendering-1.13.1.2/Graphics/Gloss/Internals/Data/Picture.hs"
            expLineNo = 60
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 6, sourceSpanEndColumn = 13, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedRunState =
        let expFile = _repos consts </> "transformers-0.5.6.2/Control/Monad/Trans/State/Lazy.hs"
            expLineNo = 109
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 9, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedPreludeReturn =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Base.hs"
            expLineNo = 862
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 5, sourceSpanEndColumn = 11, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedPrintf =
        let expFile = _repos consts </> "base-4.16.4.0/Text/Printf.hs"
            expLineNo = 257
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 7, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedTry =
        let expFile = _repos consts </> "base-4.16.4.0/Control/Exception/Base.hs"
            expLineNo = 174
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 4, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedQMap =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Internal.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 0, sourceSpanStartLine = expLineNo, sourceSpanEndLine = 0}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedDTClockPosix =
        let expFile = _repos consts </> "time-1.12.2/lib/Data/Time/Clock/POSIX.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 0, sourceSpanStartLine = expLineNo, sourceSpanEndLine = 0}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedQMapKeys =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Internal.hs"
            expLineNo = 3347
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 5, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let expectedGenerateSurface =
        let expFile = workDir </> "src/ScEaHs/Game/Surface/Generator.hs"
            expLineNo = 46
            expSrcSpan = SourceSpan {sourceSpanFileName = expFile, sourceSpanStartColumn = 1, sourceSpanEndColumn = 16, sourceSpanStartLine = expLineNo, sourceSpanEndLine = expLineNo}
         in Right $ DefinitionResponse {srcSpan = Just expSrcSpan, err = Nothing}
  let noDefErr = Left "No definition found"

  let tests =
        [ ("directly exported regular function", "playIO", expectedPlayIO),
          ("re-exported regular function", "mkStdGen", expectedMkStdGen),
          ("from prelude - function", "return", expectedPreludeReturn),
          ("from prelude - data ctor", "Nothing", expectedPreludeNothing),
          ("re-exported throughout packages (?) class name", "State", noDefErr),
          ("cross-package module re-export", "runState", expectedRunState),
          ("multiple imports of the same module", "GG.Picture", expectedPicture),
          ("(re-export)? data name", "Picture", expectedPicture),
          ("data type name", "Display", expectedDisplay),
          ("data constructor", "InWindow", expectedInWindow),
          ("data type with type variable", "Proxy", expectedProxy),
          ("in-package module re-export + operator form", "^.", expectedLensViewOperator),
          ("in-package module re-export + operator form", "%=", expectedLensOverOperator),
          ("in-package module re-export + function", "view", expectedLensView),
          ("qualified module import - go to module via qualifier", "Map", expectedQMap),
          ("qualified module import - go to module via 'original' name", "Data.Map.Strict", noDefErr),
          ("regular module import - go to module via 'original' name", "Data.Time.Clock.POSIX", expectedDTClockPosix),
          ("qualified module import - go to function through qualifier", "Map.keys", expectedQMapKeys),
          ("main-to-library resolution works", "generateSurface", expectedGenerateSurface),
          ("", "printf", expectedPrintf),
          ("", "try", expectedTry)
        ]

  describe descr $ do
    forM_ tests $ \(n, q, r) -> do
      it (n ++ " `" ++ q ++ "`") $ do
        join $ mstack (`evalStateT` serverState) $ do
          eval q r

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
  parseJSON :: Value -> Parser Version
  parseJSON (String t) = pure $ read $ unpack t

instance ToJSON Version where
  toJSON :: Version -> Value
  toJSON = String . pack . show

instance FromJSON VersionRange where
  parseJSON :: Value -> Parser VersionRange
  parseJSON (String t) = pure $ read $ unpack t

instance ToJSON VersionRange where
  toJSON :: VersionRange -> Value
  toJSON = String . pack . show

instance FromJSON Dependency

instance ToJSON Dependency

instance FromJSON PackageModules

instance ToJSON PackageModules

instance FromJSON PackageWithUnresolvedDependencies

instance ToJSON PackageWithUnresolvedDependencies

instance FromJSON PackageWithResolvedDependencies

instance ToJSON PackageWithResolvedDependencies

cabalFullTest :: Spec
cabalFullTest = do
  da <- runIO defaultArgs
  consts <- runIO $ prepareConstants da {_logLevel = LevelDebug}
  wd <- runIO getCurrentDirectory

  let descr = "cabalFull"
      wdT = wd </> "test/integrationTestRepo/sc-ea-hs"
      logF = wdT </> descr ++ ".txt"
  runIO $ removeIfExists logF

  let mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let st0 = emptyContext
  st1 <- runIO $ mstack (`execStateT` st0) Cabal.load

  let test i = do
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
        let expectedR :: [PackageWithResolvedDependencies] = fromJust $ decode expectedRS
            expectedU :: [PackageWithUnresolvedDependencies] = fromJust $ decode expectedUS
            expectedL :: [LocalPackagesKey] = fromJust $ decode expectedLS

        join $ mstack (`evalStateT` st1) $ do
          result <- runExceptT $ cabalPackage'unresolved wdT
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

  describe descr $ do
    it "test repo" $ do
      test 0

dropCacheTest :: Spec
dropCacheTest = do
  da <- runIO defaultArgs
  consts <- runIO $ prepareConstants da {_logLevel = LevelDebug}
  wd <- runIO getCurrentDirectory

  let descr = "dropCache"
      wdT = wd </> "test/integrationTestRepo/sc-ea-hs"
      mainF = wdT </> "app/game/Main.hs"
      libF = wdT </> "src/ScEaHs/Game/Surface/Generator.hs"
      logF = wdT </> descr ++ ".txt"
      reqM = DefinitionRequest {workDir = wdT, file = mainF, word = "playIO"}
      reqL = DefinitionRequest {workDir = wdT, file = libF, word = "randomR"}
  runIO $ removeIfExists logF

  let mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let st0 = emptyContext
  st1 <- runIO $ mstack (`execStateT` st0) Cabal.load
  (_, st2) <- runIO $ mstack (`runStateT` st1) $ runExceptT $ definition reqM

  describe descr $ do
    it "test repo" $ do
      x :: Either String Expectation <- mstack (`evalStateT` st2) $ runExceptT $ execWriterT $ do
        cpkgM <- cabalPackage wdT mainF
        cpkgL <- cabalPackage wdT libF

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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  c <- prepareConstants =<< defaultArgs
  removeDirectoryRecursive $ _cache c

  hspecWith defaultConfig {configPrintCpuTime = False} $ do
    haskellApplyCppHsTest
    haskellGetIdentifiersTest
    haskellGetExportsTest
    haskellGetImportsTest
    linesTest
    figureOutExportsTest
    cabalFullTest
    definitionsTest
    dropCacheTest