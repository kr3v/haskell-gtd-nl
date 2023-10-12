{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Usages where

import Control.Applicative (Applicative (..), liftA3)
import Control.Lens ((%~))
import Control.Monad (forM_, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger (LogLevel (LevelDebug))
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.ByteString.Char8 as BSC8
import Data.List (sort)
import GTD.Cabal.Cache as Cabal (load, store)
import GTD.Configuration (Args (..), GTDConfiguration (..), Powers (..), resetCache)
import GTD.Haskell.Declaration (SourceSpan (..), emptySourceSpan)
import GTD.Server.Definition (DefinitionRequest (..), definition)
import GTD.Server.Usages (usages)
import qualified GTD.Server.Usages as Usages
import GTD.State (Context, emptyContext)
import GTD.Utils (removeIfExists, up)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, beforeAll, describe, it, runIO, shouldBe)
import Text.Printf (printf)
import qualified Data.ByteString.Char8 as BSC8

instance Semigroup Usages.Response where
  (<>) :: Usages.Response -> Usages.Response -> Usages.Response
  (<>) (Usages.Response a1 b1) (Usages.Response a2 b2) = Usages.Response (a1 <> a2) (b1 <> b2)

usagesTest :: GTDConfiguration -> Spec
usagesTest consts0 = do
  let descr = "usages"

  let ps = (_powers . _args $ consts0) {_goToReferences_limit = 3, _goToReferences_isEnabled = True}
  let consts = consts0 {_args = (_args consts0) {_logLevel = LevelDebug, _powers = ps}}
  cwd <- runIO getCurrentDirectory

  let wd = cwd </> "test/integrationTestRepo/fake"
      req = DefinitionRequest {_workDir = wd, _file = mempty, _word = mempty}
      logF = wd </> descr ++ ".txt"

  let lib1 = "lib1/src/Lib1.hs"
      lib2 = "lib2/src/Lib2.hs"
      exe1 = "executables/app/exe1/Main.hs"
      exe2 = "executables/app/exe2/Main.hs"
      exe3 = "executables/app/exe3/Main.hs"
      ents = [lib1, lib2, exe1, exe2, exe3]

  let baseWd = _repos consts </> "base-4.16.4.0"

  let expectedExe3 =
        let expFile = wd </> "executables/app/exe3/Main.hs"
            expLineNo1 = 20
            expLineNo2 = 21
            expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo1, _lineEnd = expLineNo1}
            expSrcSpan2 = expSrcSpan1 {_lineBegin = expLineNo2, _lineEnd = expLineNo2}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1, expSrcSpan2], Usages._err = Nothing}
  let expectedReturn =
        let expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "executables/app/exe3/Main.hs", _colBegin = 8, _colEnd = 14, _lineBegin = 21, _lineEnd = 21}
            expSrcSpan2 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib2/src/Lib2.hs", _colBegin = 8, _colEnd = 14, _lineBegin = 4, _lineEnd = 4}
            expSrcSpan3 = emptySourceSpan {_fileName = BSC8.pack $ _repos consts </> "base-4.16.4.0/Data/Fixed.hs", _colBegin = 5, _colEnd = 11, _lineBegin = 237, _lineEnd = 237}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1, expSrcSpan2, expSrcSpan3], Usages._err = Nothing}
  let expectedNothing = Right $ Usages.Response {Usages._srcSpan = [], Usages._err = Nothing}

  let expectedLib1Decls =
        let expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib1/src/Lib1.hs", _colBegin = 1, _colEnd = 5, _lineBegin = 6, _lineEnd = 6}
            expSrcSpan2 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib1/src/Lib1.hs", _colBegin = 1, _colEnd = 5, _lineBegin = 7, _lineEnd = 7}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1, expSrcSpan2], Usages._err = Nothing}
  let expectedLib2Decls =
        let expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib2/src/Lib2.hs", _colBegin = 1, _colEnd = 5, _lineBegin = 3, _lineEnd = 3}
            expSrcSpan2 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib2/src/Lib2.hs", _colBegin = 1, _colEnd = 5, _lineBegin = 4, _lineEnd = 4}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1, expSrcSpan2], Usages._err = Nothing}
  let expectedLib1Exports =
        let expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib1/src/Lib1.hs", _colBegin = 14, _colEnd = 18, _lineBegin = 1, _lineEnd = 1}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1], Usages._err = Nothing}
  let expectedLib2Exports =
        let expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib1/src/Lib1.hs", _colBegin = 20, _colEnd = 24, _lineBegin = 1, _lineEnd = 1}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1], Usages._err = Nothing}
  let expectedLib2Imports =
        let expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib1/src/Lib1.hs", _colBegin = 14, _colEnd = 18, _lineBegin = 4, _lineEnd = 4}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1], Usages._err = Nothing}
  let expectedLib1Data11 =
        let expSrcSpan1 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib1/src/Lib1.hs", _colBegin = 9, _colEnd = 21, _lineBegin = 11, _lineEnd = 11}
            expSrcSpan2 = emptySourceSpan {_fileName = BSC8.pack $ wd </> "lib1/src/Lib1.hs", _colBegin = 9, _colEnd = 21, _lineBegin = 18, _lineEnd = 18}
         in Right $ Usages.Response {Usages._srcSpan = [expSrcSpan1, expSrcSpan2], Usages._err = Nothing}

  let importsE = ps {_goToReferences_shouldIncludeImports = True}
  let exportsE = ps {_goToReferences_shouldIncludeExports = True}
  let declsE = ps {_goToReferences_shouldIncludeDeclarations = True}
  let allE =
        ps
          { _goToReferences_shouldIncludeImports = True,
            _goToReferences_shouldIncludeExports = True,
            _goToReferences_shouldIncludeDeclarations = True,
            _goToReferences_limit = 5
          }

  let tests =
        [ (ps, exe3, wd, wd, "local function", "exe3", expectedNothing),
          (ps, exe3, wd, wd, "Prelude function", "return", expectedReturn),
          (ps, "Prelude.hs", baseWd, wd, "Prelude function", "return", expectedReturn),
          (ps, lib1, wd, wd, "default", "lib1", expectedNothing),
          (ps, lib1, wd, wd, "default", "lib2", expectedNothing),
          (importsE, lib1, wd, wd, "imports=True", "lib1", expectedNothing),
          (importsE, lib1, wd, wd, "imports=True", "lib2", expectedLib2Imports),
          (exportsE, lib1, wd, wd, "exports=True", "lib1", expectedLib1Exports),
          (exportsE, lib1, wd, wd, "exports=True", "lib2", expectedLib2Exports),
          (declsE, lib1, wd, wd, "decl=True", "lib1", expectedLib1Decls),
          (declsE, lib1, wd, wd, "decl=True", "lib2", expectedLib2Decls),
          (allE, lib1, wd, wd, "all=True", "lib1", (Usages.srcSpan %~ sort) <$> liftA2 (<>) expectedLib1Decls expectedLib1Exports),
          (allE, lib1, wd, wd, "all=True", "lib2", (Usages.srcSpan %~ sort) <$> liftA3 (up (<>)) expectedLib2Decls expectedLib2Exports expectedLib2Imports),
          (ps, lib1, wd, wd, "default", "lib1_data1_1", expectedNothing),
          (allE, lib1, wd, wd, "default", "lib1_data1_1", expectedLib1Data11)
        ]

  let evalD f w = runExceptT $ definition req {_workDir = wd, _file = wd </> f, _word = w}
      eval f wd owd w exp = do
        let rq = Usages.Request {Usages._origWorkDir = owd, Usages._workDir = wd, Usages._file = wd </> f, Usages._word = w}
        got <- fmap (Usages.srcSpan %~ sort) <$> runExceptT (usages rq)
        liftIO $ printf "%s -> %s ?= %s\n" (show rq) (show got) (show exp)
        return $ got `shouldBe` exp
      mstack c f a = runFileLoggingT logF $ f $ runReaderT a c

  let init :: IO Context = do
        print descr
        printf "cwd = %s, wd = %s, logF = %s\n" cwd wd logF
        removeIfExists logF
        runFileLoggingT logF $ runReaderT resetCache consts
        mstack consts (`execStateT` emptyContext) $ Cabal.load >> (evalD exe3 (BSC8.pack "return") >>= liftIO . printf "%s %s %s -> %s\n" descr exe3 "return" . show) >> Cabal.store

  beforeAll init $ describe descr $ forM_ tests $ \(ps, f, wd, owd, n, q, r) -> do
    it (printf "n=%s, f=%s, q=`%s`" n f q) $ \ss -> do
      join $ mstack (consts {_args = (_args consts) {_powers = ps}}) (`evalStateT` ss) $ do
        eval f wd owd (BSC8.pack q) r
