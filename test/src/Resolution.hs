{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Resolution where

import Control.Monad (forM, forM_, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.RWS (MonadState (get))
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Either (partitionEithers)
import Data.Either.Combinators (mapLeft, mapRight)
import qualified Data.HashMap.Strict as HMap
import Data.List (isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Haskell.Module (HsModule (..), HsModuleMetadata (_mPath), HsModuleP (..), emptyMetadata, parseModule)
import GTD.Resolution.Module (ModuleState (..), figureOutExports, figureOutExports'old)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)
import Text.Printf (printf)

figureOutExportsTest :: GTDConfiguration -> Spec
figureOutExportsTest consts = do
  let descr = "figureOutExports"
      root = "test/samples" </> descr
  tests <- runIO $ listDirectory root
  describe descr $ forM_ tests $ \test -> do
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
        mapRight (p,) . mapLeft (printf "failed to parse %s: %s" p) <$> runExceptT (flip runReaderT consts $ parseModule emptyMetadata {_mPath = p})
      let (errors :: [String], importedModules :: [(String, HsModule)]) = partitionEithers importsE
      liftIO $ print errors

      mainModuleE <- runStdoutLoggingT $ flip runReaderT consts $ runExceptT $ parseModule emptyMetadata {_mPath = mainFileP}
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