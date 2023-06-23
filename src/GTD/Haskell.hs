{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell where

import Control.Exception (ErrorCall, Exception (displayException), IOException, try)
import qualified Control.Exception as Exc
import Control.Exception.Safe (tryAny)
import Control.Lens (At (..), Ixed (..), makeLenses, use, (%=), (&), (.=), (^.))
import Control.Monad (forM, forM_, guard, unless, when)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logDebugN, logDebugNS, logDebugSH)
import Control.Monad.State (MonadIO (liftIO), MonadState, StateT (..), evalStateT, execStateT, get, modify)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (MonadWriter, WriterT (..), execWriter, execWriterT, lift, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.UTF8 (fromString)
import Data.Data (Data (..), showConstr)
import Data.Either (partitionEithers)
import Data.Either.Combinators (mapLeft)
import Data.Functor ((<&>))
-- import GTD.Haskell.Enrich (enrichTryPackage)

import qualified Data.Graph as Graph
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.ModuleName as Cabal
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import GTD.Cabal (CabalLibSrcDir, CabalPackage (..), ModuleNameS, PackageNameS, cabalPackageExportedModules, cabalPackageName, haskellPath)
import GTD.Haskell.AST (haskellGetExportedIdentifiers, haskellGetImportedIdentifiers)
import GTD.Haskell.Declaration (Declaration (..), Identifier (Identifier), hasNonEmptyOrig)
import GTD.Haskell.Enrich (enrichTryModule, enrichTryPackage)
import GTD.Haskell.Module (HsModule (..))
import qualified GTD.Haskell.Module as HSM
import GTD.Haskell.Utils
import GTD.Haskell.Walk (MS (MS), parseChosenModules)
import qualified GTD.Haskell.Walk as MS
import GTD.Utils (flipTuple, logDebugNSS, logErrorNSS, maybeToMaybeT, tryE, withExceptT)
import Language.Haskell.Exts (Decl (..), ExportSpec (..), ExportSpecList (..), ImportDecl (..), ImportSpec (..), ImportSpecList (..), Module (..), ModuleHead (..), ModuleName (..), Name (..), ParseMode (..), ParseResult (..), QName (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, parseFile, parseFileContents, parseFileContentsWithMode, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import Text.Printf (printf)

data ContextCabalPackage = ContextCabalPackage
  { _ccpmodules :: Map.Map PackageNameS (Map.Map ModuleNameS HsModule),
    _dependencies :: [CabalPackage]
  }
  deriving (Show, Generic)

$(makeLenses ''ContextCabalPackage)

parsePackages :: (MonadLoggerIO m, MonadState ContextCabalPackage m) => m ()
parsePackages = do
  deps <- use dependencies
  forM_ deps $ \dep -> do
    mods <- parsePackage dep
    ccpmodules %= Map.insert (_cabalPackageName dep) mods

parsePackage0 ::
  CabalPackage ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModule)
parsePackage0 p = do
  let logTag = "parse package 0"
  let ms = MS Map.empty Set.empty
  ms' <- execStateT (parseChosenModules p (Map.keys $ _cabalPackageExportedModules p)) ms
  return $ MS._modules ms'

parsePackage ::
  CabalPackage ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModule)
parsePackage p = do
  let logTag = "parse package 1"
  modules <- parsePackage0 p

  let iModules :: Map.Map Int HsModule = Map.fromList $ zip [1 ..] (Map.elems modules)
  let iNModules = Map.fromList $ flipTuple <$> Map.assocs (_name <$> iModules)

  let jF :: Int -> ModuleNameS -> (Int, Int)
      jF i dep = (i, fromJust $ Map.lookup dep iNModules)
      iF :: Int -> HsModule -> [(Int, Int)]
      iF i mod = jF i <$> filter (`Map.member` iNModules) (_deps mod)
  let graph = Graph.buildG (1, Map.size modules) $ concatMap (uncurry iF) (Map.assocs iModules)
  let graphS = Graph.reverseTopSort graph

  let oModules = (\i -> fromJust $ Map.lookup i iModules) <$> graphS

  flip execStateT Map.empty $ forM_ oModules $ \mod -> do
    logDebugNSS logTag $ _name mod

    (isImplicitExportAll, exports) <- runWriterT $ haskellGetExportedIdentifiers (_ast mod)
    imports <- execWriterT $ haskellGetImportedIdentifiers (_ast mod)
    let locals = _decls mod

    st <- get
    let importsE = (\d -> fromMaybe d (enrichTryModule st d)) <$> imports

    -- drop `isImplicitExportAll then locals` cases from `topSort` invocation to avoid circular dependencies (cycles)
    let m = mod {_exports = if isImplicitExportAll then locals else Map.intersection (asDeclsMap exports) (locals <> asDeclsMap importsE)}
    modify $ Map.insert (_name mod) m

---

enrich0 :: Declaration -> (MonadLogger m, MonadState ContextCabalPackage m) => m Declaration
enrich0 d = do
  let logTag = "enrich"
  deps <- use dependencies
  mods <- use ccpmodules
  let xs = filter hasNonEmptyOrig $ mapMaybe (enrichTryPackage d mods) deps
  case length xs of
    0 -> return d
    1 -> return $ head xs
    _ -> do
      logDebugNSS logTag $ printf "multiple matches for %s: %s" (show d) (show xs)
      return $ head xs

enrich :: HsModule -> (MonadLoggerIO m, MonadState ContextCabalPackage m) => m HsModule
enrich mod = do
  (isImplicitExportAll, exports) <- runWriterT $ haskellGetExportedIdentifiers (_ast mod)
  imports <- execWriterT $ haskellGetImportedIdentifiers (_ast mod)
  let locals = _decls mod
  importsE <- mapM enrich0 imports
  return $ mod {_decls = locals <> asDeclsMap importsE}
