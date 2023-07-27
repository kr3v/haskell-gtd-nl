{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative (Alternative (empty), Applicative (liftA2))
import Control.Exception (IOException, evaluate, try)
import Control.Lens (At (at), use)
import Control.Lens.Prism (_Just)
import Control.Monad.Logger (NoLoggingT (runNoLoggingT), runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.State (MonadState (..), MonadTrans (lift), StateT (..), evalStateT, execStateT, forM)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Writer (MonadIO (liftIO), execWriterT, forM_, join, runWriterT)
import Data.Aeson (FromJSON, ToJSON, decode, defaultOptions, encode, genericToJSON)
import qualified Data.ByteString.Lazy as BS
import Data.Either (partitionEithers)
import Data.Either.Combinators (mapLeft, mapRight)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (diffUTCTime)
import Data.Time.Clock.POSIX (getCurrentTime)
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription (emptyGenericPackageDescription, emptyPackageDescription)
import GHC.RTS.Flags (ProfFlags (descrSelector))
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (_repos), prepareConstants)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declaration (Declaration, _declName, _declSrcOrig), Declarations (..), Exports, Identifier (..), Imports, SourceSpan (SourceSpan, sourceSpanEndColumn, sourceSpanEndLine, sourceSpanFileName, sourceSpanStartColumn, sourceSpanStartLine), emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Resolution.Module (figureOutExports, figureOutExports0)
import GTD.Resolution.State (emptyContext)
import GTD.Resolution.State.Caching.Cabal (cabalCacheFetch, cabalCacheStore)
import GTD.Server (DefinitionRequest (..), DefinitionResponse (..), definition, noDefintionFoundError, noDefintionFoundErrorE)
import GTD.Utils (logDebugNSS, ultraZoom)
import Language.Haskell.Exts (Module (..), Parseable (parse), SrcSpan (..), SrcSpanInfo (..))
import System.Directory (getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO
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

haskellGetIdentifiersSpec :: Spec
haskellGetIdentifiersSpec = do
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

        result <- liftIO $ p srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right identifiersM -> do
            identifiers <- identifiersM
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  let ghcP a b = mapRight (runStderrLoggingT . execWriterT . GHC.identifiers) <$> GHC.parse a b
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

haskellGetExportsSpec :: Spec
haskellGetExportsSpec = do
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

  let ghcP a b = mapRight (runStderrLoggingT . flip execStateT Map.empty . GHC.exports) <$> GHC.parse a b
  let parsers = [("ghc-lib-parser", ghcP)]

  forM_ parsers $ \(n, p) -> do
    describe descr $ do
      describe n $ do
        it "parses a file with an explicit list of exported functions" $
          test n p 0

haskellGetImportsSpec :: Spec
haskellGetImportsSpec = do
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

  let ghcP a b = mapRight (runStderrLoggingT . execWriterT . GHC.imports) <$> GHC.parse a b
  let parsers = [("ghc-lib-parser", ghcP)]

  forM_ parsers $ \(n, p) -> do
    describe descr $ do
      describe n $ do
        it "extracts only function imports" $
          test n p 0

---

figureOutExportsTest :: Spec
figureOutExportsTest = do
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
          Left err -> return $ expectationFailure $ printf "failed to parse %s: %s" mainFileP err
          Right mainModule -> do
            liftIO $ runStdoutLoggingT $ flip evalStateT Map.empty $ do
              forM_ importedModules $ \(p, m) -> do
                figureOutExports m
                liftIO $ BS.writeFile (p ++ ".out.json") $ encode m
              st0 <- get
              liftIO $ BS.writeFile outS0P $ encode st0
              (result, _, debugInfo) <- figureOutExports0 st0 mainModule
              liftIO $ BS.writeFile outDP $ encode debugInfo
              liftIO $ BS.writeFile outMP $ encode mainModule
              liftIO $ BS.writeFile outRP $ encode result
              return $ result `shouldBe` expectedR

---

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
  let noDefErr = Left "No definition found"

  let st0 = emptyContext
  st1 <- runIO $ mstack execStateT st0 cabalCacheFetch
  (a, serverState) <- runIO $ mstack runStateT st1 $ eval0 "playIO"
  runIO $ print a
  runIO $ mstack execStateT serverState cabalCacheStore

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

    -- if `return` is broken, then it is possible that `module X (module X) where ...` case is broken
    it "from prelude - function" $ do
      join $ mstack evalStateT serverState $ do
        eval "return" expectedPreludeReturn
    it "from prelude - data ctor" $ do
      join $ mstack evalStateT serverState $ do
        eval "Nothing" expectedPreludeNothing

    it "re-exported throughout packages (?) class name" $ do
      join $ mstack evalStateT serverState $ do
        eval "State" noDefErr
    it "cross-package module re-export" $ do
      join $ mstack evalStateT serverState $ do
        eval "runState" expectedRunState
    it "(re-export)? data name" $ do
      join $ mstack evalStateT serverState $ do
        eval "Picture" expectedPicture

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

integrationTestsSpec :: Spec
integrationTestsSpec = do
  definitionsSpec

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  hspecWith defaultConfig {configPrintCpuTime = False} $ do
    haskellApplyCppHsSpec
    haskellGetIdentifiersSpec
    haskellGetExportsSpec
    haskellGetImportsSpec
    integrationTestsSpec
    figureOutExportsTest
