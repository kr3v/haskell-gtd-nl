{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Definitions where

import Control.Monad (forM_, join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Logger.CallStack (runFileLoggingT)
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))
import qualified Data.ByteString.Char8 as BSC8
import GTD.Cabal.Cache as Cabal (load, store)
import GTD.Configuration (GTDConfiguration (..))
import GTD.Haskell.Declaration (SourceSpan (SourceSpan, _colEnd, _lineEnd, _fileName, _colBegin, _lineBegin))
import GTD.Server.Definition (DefinitionRequest (..), DefinitionResponse (..), definition)
import GTD.State (emptyContext)
import GTD.Utils (removeIfExists)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import Text.Printf (printf)


definitionTests :: GTDConfiguration -> Spec
definitionTests consts = do
  cwd <- runIO getCurrentDirectory

  let descr = "definitions"
      wd = cwd </> "test/integrationTestRepo/fake"
      req = DefinitionRequest {_workDir = wd, _file = "", _word = ""}
      logF = wd </> descr ++ ".txt"

  runIO $ print descr
  runIO $ printf "cwd = %s, wd = %s, logF = %s\n" cwd wd logF
  runIO $ removeIfExists logF

  let eval0 f w = runExceptT $ definition req {_file = wd </> f, _word = w}
      eval f w r = eval0 f w >>= (\d -> return $ d `shouldBe` r)
      mstack f a = runFileLoggingT logF $ f $ runReaderT a consts

  let lib1 = "lib1/src/Lib1.hs"
      lib2 = "lib2/src/Lib2.hs"
      exe1 = "executables/app/exe1/Main.hs"
      exe2 = "executables/app/exe2/Main.hs"
      exe3 = "executables/app/exe3/Main.hs"
      ents = [lib1, lib2, exe1, exe2, exe3]
  serverState <- runIO $ mstack (`execStateT` emptyContext) $ Cabal.load >> (eval0 lib1 "return" >>= liftIO . print) >> Cabal.store

  let expectedPreludeReturn =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Base.hs"
            expLineNo = 862
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 5, _colEnd = 11, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedLib1 =
        let expFile = wd </> "lib1/src/Lib1.hs"
            expLineNo = 6
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedLib2 =
        let expFile = wd </> "lib2/src/Lib2.hs"
            expLineNo = 3
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedExe1 =
        let expFile = wd </> "executables/app/exe1/Main.hs"
            expLineNo = 6
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedExe2 =
        let expFile = wd </> "executables/app/exe2/Main.hs"
            expLineNo = 6
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedExe3 =
        let expFile = wd </> "executables/app/exe3/Main.hs"
            expLineNo = 20
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedLib2ReexportedTypeAlias =
        let expFile = wd </> "lib2/src/Lib2.hs"
            expLineNo = 6
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 6, _colEnd = 30, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}

      expectedMkStdGen =
        let expFile = _repos consts </> "random-1.2.1.1/src/System/Random/Internal.hs"
            expLineNo = 582
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 9, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedLensView =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Getter.hs"
            expLineNo = 244
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedLensViewOperator =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Getter.hs"
            expLineNo = 316
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedLensOverOperator =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Setter.hs"
            expLineNo = 792
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedProxy =
        let expFile = _repos consts </> "base-4.16.4.0/Data/Proxy.hs"
            expLineNo = 56
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 16, _colEnd = 21, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedPreludeNothing =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Maybe.hs"
            expLineNo = 29
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 19, _colEnd = 26, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedRunState =
        let expFile = _repos consts </> "transformers-0.5.6.2/Control/Monad/Trans/State/Lazy.hs"
            expLineNo = 109
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 9, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedPrintf =
        let expFile = _repos consts </> "base-4.16.4.0/Text/Printf.hs"
            expLineNo = 257
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 7, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedTry =
        let expFile = _repos consts </> "base-4.16.4.0/Control/Exception/Base.hs"
            expLineNo = 174
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 4, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedQMap =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Internal.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 1, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedDTClockPosix =
        let expFile = _repos consts </> "time-1.12.2/lib/Data/Time/Clock/POSIX.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 1, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedQMapKeys =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Internal.hs"
            expLineNo = 3347
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 5, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedGetRTSStats =
        let expFile = _repos consts </> "base-4.16.4.0/GHC/Stats.hsc"
            expLineNo = 190
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 12, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedState =
        let expFile = _repos consts </> "transformers-0.5.6.2/Control/Monad/Trans/State/Lazy.hs"
            expLineNo = 97
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 6, _colEnd = 11, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedDataMapStrict =
        let expFile = _repos consts </> "containers-0.6.7/src/Data/Map/Strict.hs"
            expLineNo = 1
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 1, _colEnd = 1, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedGetter =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Type.hs"
            expLineNo = 490
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 6, _colEnd = 12, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedConst =
        let expFile = _repos consts </> "base-4.18.1.0/Data/Functor/Const.hs"
            expLineNo = 39
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 21, _colEnd = 26, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedContravariant =
        let expFile = _repos consts </> "base-4.18.1.0/Data/Functor/Contravariant.hs"
            expLineNo = 99
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 7, _colEnd = 20, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedSetter =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Type.hs"
            expLineNo = 292
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 6, _colEnd = 12, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedSettable =
        let expFile = _repos consts </> "lens-5.2.3/src/Control/Lens/Internal/Setter.hs"
            expLineNo = 32
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 57, _colEnd = 65, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
      expectedIdentity =
        let expFile = _repos consts </> "base-4.18.1.0/Data/Functor/Identity.hs"
            expLineNo = 57
            expSrcSpan = SourceSpan {_fileName = BSC8.pack expFile, _colBegin = 22, _colEnd = 30, _lineBegin = expLineNo, _lineEnd = expLineNo}
         in Right $ DefinitionResponse {_srcSpan = [expSrcSpan], _err = Nothing}
  let noDefErr = Right $ DefinitionResponse {_srcSpan = [], _err = Nothing}

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

  describe descr $ forM_ tests $ \(f, n, q, r) -> do
    it (printf "n=%s, f=%s, q=`%s`" n f q) $ do
      join $ mstack (`evalStateT` serverState) $ do
        eval f q r
