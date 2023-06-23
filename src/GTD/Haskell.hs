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
import Control.Lens (At (..), makeLenses, use, (%=), (&), (.=), (^.))
import Control.Monad (forM, forM_, guard, unless, when)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, logDebugN, logDebugNS, logDebugSH)
import Control.Monad.State (MonadIO (liftIO), MonadState, StateT (..), evalStateT, execStateT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (MonadWriter, WriterT (..), execWriterT, lift, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.UTF8 (fromString)
import Data.Data (Data (..), showConstr)
import Data.Either (partitionEithers)
import Data.Either.Combinators (mapLeft)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Distribution.ModuleName as Cabal
import GHC.Generics (Generic)
import GHC.IO.Unsafe (unsafePerformIO)
import GTD.Cabal (CabalLibSrcDir, CabalPackage (..), ModuleNameS, PackageNameS, cabalPackageExportedModules, cabalPackageName, haskellPath)
import GTD.Haskell.Declaration (Declaration (_declName), Identifier (Identifier), hasNonEmptyOrig)
import GTD.Haskell.Enrich (enrichTryPackage)
import GTD.Haskell.Module (HsModule)
import GTD.Haskell.Walk (MS (MS), parseChosenModules)
import qualified GTD.Haskell.Walk as MS
import GTD.Utils (logDebugNSS, logErrorNSS, maybeToMaybeT, tryE, withExceptT)
import Language.Haskell.Exts (Decl (..), ExportSpec (..), ExportSpecList (..), ImportDecl (..), ImportSpec (..), ImportSpecList (..), Module (..), ModuleHead (..), ModuleName (..), Name (..), ParseMode (..), ParseResult (..), QName (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, parseFile, parseFileContents, parseFileContentsWithMode, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import Text.Printf (printf)

data ContextCabalPackage = ContextCabalPackage
  { _modules :: Map.Map PackageNameS (Map.Map ModuleNameS HsModule),
    _dependencies :: [CabalPackage]
  }
  deriving (Show, Generic)

$(makeLenses ''ContextCabalPackage)

---

enrich :: Declaration -> (MonadLogger m, MonadState ContextCabalPackage m) => m Declaration
enrich d = do
  let logTag = "enrich"
  deps <- use dependencies
  mods <- use modules
  xs <- fmap (filter hasNonEmptyOrig . catMaybes) <$> forM deps $ runMaybeT . enrichTryPackage d mods
  case length xs of
    0 -> return d
    1 -> return $ head xs
    _ -> do
      logDebugNSS logTag $ printf "multiple matches for %s: %s" (show d) (show xs)
      return $ head xs

---

parsePackages :: (MonadLoggerIO m, MonadState ContextCabalPackage m) => m ()
parsePackages = do
  deps <- use dependencies
  forM_ deps $ \dep -> parsePackage dep

parsePackage ::
  CabalPackage ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModule)
parsePackage p = do
  let logTag = "parse package"
  let ms = MS Map.empty Set.empty
  ms' <- execStateT (parseChosenModules p (Map.keys $ _cabalPackageExportedModules p)) ms
  return $ MS._modules ms'

---

-- parseModulePhase2 ::
--   Bool ->
--   FilePath ->
--   HsModule ->
--   (MonadLoggerIO m, MonadState ContextCabalPackage m) => ExceptT String m HsModule
-- parseModulePhase2 shouldEnrich srcP cm = do
--   let logTag = printf "parsing module %s (enrich=%s)" srcP (show shouldEnrich)
--   logDebugNSS logTag $ printf ""

--   imports <- execWriterT (haskellGetImportedIdentifiers (_cmodule cm)) >>= mapM (if shouldEnrich then enrich else return)

--   logDebugNSS logTag "imports:"
--   forM_ imports $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

--   return $ cm {_decls = Map.union (_locals cm) (asDeclsMap imports)}

-- parseModulePhase3 ::
--   FilePath ->
--   HsModule ->
--   (MonadLoggerIO m, MonadState ContextCabalPackage m) => ExceptT String m HsModule
-- parseModulePhase3 srcP cm = do
--   let logTag = printf "parsing module 2 - %s" srcP

--   logDebugNSS logTag "exports:"
--   (isImplicitExportAll, exports) <- runWriterT $ haskellGetExportedIdentifiers (_cmodule cm)
--   forM_ exports $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

--   return $ cm {_exports = if isImplicitExportAll then _locals cm else Map.intersection (asDeclsMap exports) (_identifiers cm)}

---
