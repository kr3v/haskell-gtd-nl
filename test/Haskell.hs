{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}

module Haskell where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (NoLoggingT (..), runStderrLoggingT)
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Writer (execWriterT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Either.Combinators (mapRight)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import GHC.Generics (Generic)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declarations (..), Exports, IdentifierWithUsageLocation (..), Imports, UsageType)
import GTD.Haskell.Lines (Line (..), buildMap, resolve)
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Utils (mapDFrom, storeIOExceptionToMonadError)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import Text.Printf (printf)

haskellGetIdentifiersTest :: GTDConfiguration -> Spec
haskellGetIdentifiersTest consts = do
  let descr = "haskellGetIdentifiers"
  let test n p i = do
        let iS = show i
            srcPath = "test/samples" </> descr </> ("in." ++ iS ++ ".hs")
            dstPath = "test/samples" </> descr </> ("out." ++ n ++ "." ++ iS ++ ".json")
            expPath = "test/samples" </> descr </> ("exp." ++ iS ++ ".json")

        src <- readFile srcPath
        expectedS <- try (BS.readFile expPath) :: IO (Either IOException BS.ByteString)
        let expected :: Declarations =
              case expectedS of
                Left _ -> mempty
                Right s -> do
                  let m :: Maybe Declarations = decode s
                  fromMaybe mempty m

        result <- runFileLoggingT ("test/samples" </> descr </> "loggingT." ++ iS ++ ".log") $ p srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right identifiers -> do
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  let ghcP a b = mapM (execWriterT . GHC.identifiers) =<< runNoLoggingT (GHC.parse a b)
  let parsers = [("ghc-lib-parser", ghcP)]

  forM_ parsers $ \(n, p) -> describe descr $ do
    describe n $ do
      it "parses only function declarations" $
        test n p 0
      it "parses declarations of multiple functions with shared type signature" $
        test n p 1
      it "parses classes declarations" $
        test n p 2
      it "tycd only plus multiple pattern matches" $
        test n p 3
      it "duplicate field name" $
        test n p 4

haskellGetExportsTest :: GTDConfiguration -> Spec
haskellGetExportsTest consts = do
  let descr = "haskellGetExportedIdentifiers"
  let test n p i = do
        let iS = show i
            srcPath = "test/samples" </> descr </> ("in." ++ iS ++ ".hs")
            dstPath = "test/samples" </> descr </> ("out." ++ n ++ "." ++ iS ++ ".json")
            expPath = "test/samples" </> descr </> ("exp." ++ iS ++ ".json")

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

  forM_ parsers $ \(n, p) -> describe descr $ do
    describe n $ do
      it "parses a file with an explicit list of exported functions" $
        test n p 0

haskellGetImportsTest :: GTDConfiguration -> Spec
haskellGetImportsTest consts = do
  let descr = "haskellGetImportedIdentifiers"
  let test n p i = do
        let iS = show i
            srcPath = "test/samples" </> descr </> ("in." ++ iS ++ ".hs")
            dstPath = "test/samples" </> descr </> ("out." ++ n ++ "." ++ iS ++ ".json")
            expPath = "test/samples" </> descr </> ("exp." ++ iS ++ ".json")

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

  forM_ parsers $ \(n, p) -> describe descr $ do
    describe n $ do
      it "extracts only function imports" $
        test n p 0

haskellGetIdentifierUsagesTest :: GTDConfiguration -> Spec
haskellGetIdentifierUsagesTest consts = do
  let descr = "haskellGetIdentifierUsages"
  let test n p i = do
        let iS = show i
            srcPath = "test/samples" </> descr </> ("in." ++ iS ++ ".hs")
            dstPath = "test/samples" </> descr </> ("out." ++ n ++ "." ++ iS ++ ".json")
            expPath = "test/samples" </> descr </> ("exp." ++ iS ++ ".json")

        src <- readFile srcPath
        expectedS <- BS.readFile expPath
        let expected :: Map.Map UsageType [IdentifierWithUsageLocation] = fromJust $ decode expectedS

        result <- p srcPath src
        case result of
          Left e -> expectationFailure $ printf "failed to parse %s: %s" srcPath e
          Right identifiers -> do
            BS.writeFile dstPath $ encode identifiers
            identifiers `shouldBe` expected

  let ghcP a b = mapRight (mapDFrom _iuType . GHC.identifierUsages'raw) <$> runNoLoggingT (GHC.parse a b)
  let parsers = [("ghc-lib-parser", ghcP)]

  forM_ parsers $ \(n, p) -> describe descr $ do
    describe n $ do
      it "parses a file with an explicit list of exported functions" $
        test n p 0
      it "parses a file with an explicit list of exported functions" $
        test n p 1

haskellApplyCppHsTest :: GTDConfiguration -> Spec
haskellApplyCppHsTest consts = describe "haskellApplyCppHs" $ do
  it "applies #if-related C preprocessor directives" $ do
    let srcPath = "test/samples/haskellApplyCppHs/in.0.hs"
    let dstPath = "test/samples/haskellApplyCppHs/out.0.hs"
    let expPath = "test/samples/haskellApplyCppHs/exp.0.hs"

    src <- readFile srcPath
    expected <- readFile expPath

    result <- liftIO $ runExceptT $ haskellApplyCppHs srcPath src
    case result of
      Left e -> expectationFailure $ show e
      Right r -> do
        writeFile dstPath r
        r `shouldBe` expected
  it "ignores #error directive" $ do
    let srcPath = "test/samples/haskellApplyCppHs/in.1.hs"
    let dstPath = "test/samples/haskellApplyCppHs/out.1.hs"
    let expPath = "test/samples/haskellApplyCppHs/exp.1.hs"

    src <- readFile srcPath
    expected <- readFile expPath

    result <- liftIO $ runExceptT $ haskellApplyCppHs srcPath src
    case result of
      Left e -> expectationFailure $ show e
      Right r -> do
        writeFile dstPath r
        r `shouldBe` expected

---

data LinesSpecTestCase = LinesSpecTestCase
  { _in :: Line,
    _out :: Line
  }
  deriving (Show, Generic)

instance FromJSON LinesSpecTestCase

instance ToJSON LinesSpecTestCase

linesTest :: GTDConfiguration -> Spec
linesTest consts = do
  let descr = "lines"
  let test i = runExceptT $ do
        let iS = show i
            sampleRoot = "test/samples" </> descr
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

  describe descr $ it "???" $ do
    x <- test 0
    either (expectationFailure . show) id x
