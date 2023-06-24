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
import Control.Lens (At (..), Ixed (..), makeLenses, use, view, (%=), (&), (.=), (^.))
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
import GTD.Haskell.AST (Exports (..), Imports (..), haskellGetExports, haskellGetImports)
import GTD.Haskell.Declaration (Declaration (..), Identifier (Identifier), hasNonEmptyOrig)
import GTD.Haskell.Enrich (enrichTryModule, enrichTryPackage)
import GTD.Haskell.Module (HsModule (..))
import qualified GTD.Haskell.Module as HSM
import GTD.Haskell.Utils (asDeclsMap)
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
  mods <- use ccpmodules
  forM_ deps $ \dep -> do
    unless (Map.member (_cabalPackageName dep) mods) $ do
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

  execStateT (forM_ oModules moduleEvalExports) Map.empty

---

getExportedModule ::
  ContextCabalPackage ->
  ModuleNameS ->
  Either String HsModule
getExportedModule ContextCabalPackage {_ccpmodules = mods, _dependencies = deps} modN = do
  let modules = flip mapMaybe deps $ \pkg -> do
        guard $ modN `Map.member` _cabalPackageExportedModules pkg
        dependencyModules <- _cabalPackageName pkg `Map.lookup` mods
        modN `Map.lookup` dependencyModules
  case length modules of
    0 -> Left $ printf "no package seem to export" (show modN)
    1 -> Right $ head modules
    _ -> Left $ printf "multiple matches for %s: %s" (show modN) (show $ _name <$> modules)

getAllImports :: HsModule -> (MonadLoggerIO m, MonadState ContextCabalPackage m) => m [Declaration]
getAllImports mod = do
  let logTag = "get all imports for " <> show (_name mod)
  Imports importsS importedModules <- execWriterT $ haskellGetImports (_ast mod)
  ctx <- get
  let (errorsM, importsM) = partitionEithers $ getExportedModule ctx <$> importedModules
  forM_ errorsM $ \err -> logErrorNSS logTag err
  return $ importsS ++ concatMap (Map.elems . _exports) importsM

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
  let logTag = "module enrich for " ++ _name mod
  (isImplicitExportAll, exports) <- runWriterT $ haskellGetExports (_ast mod)
  importsE <- getAllImports mod >>= mapM enrich0
  return $ mod {_decls = _decls mod <> asDeclsMap importsE}

---

moduleEvalExports :: HsModule -> (MonadLoggerIO m, MonadState (Map.Map ModuleNameS HsModule) m) => m ()
moduleEvalExports mod = do
  let logTag = "module prepare exports for " ++ _name mod
  logDebugNSS logTag $ _name mod
  let locals = _decls mod

  st <- get
  (isImplicitExportAll, Exports exportsS exportsM) <- runWriterT $ haskellGetExports (_ast mod)
  let exports = exportsS ++ concatMap (Map.elems . _exports) (mapMaybe (\n -> view (at n) st) exportsM)

  Imports importsS importsM <- execWriterT $ haskellGetImports (_ast mod)
  let imports = importsS ++ concatMap (Map.elems . _exports) (mapMaybe (\n -> view (at n) st) importsM)
  let importsE = fmap (\d -> fromMaybe d (enrichTryModule st d)) imports

  -- drop `isImplicitExportAll then locals` cases from `topSort` invocation to avoid circular dependencies (cycles)
  let m = mod {_exports = if isImplicitExportAll then locals else Map.intersection (locals <> asDeclsMap importsE) (asDeclsMap exports)}
  modify $ Map.insert (_name mod) m
