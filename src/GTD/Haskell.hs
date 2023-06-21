{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Haskell where

import Control.Exception
import Control.Lens (At (..), makeLenses, use, (%=), (&), (^.))
import Control.Monad (forM, forM_, guard, unless, when)
import Control.Monad.Logger (MonadLogger, logDebugN, logDebugNS, logDebugSH)
import Control.Monad.State (MonadIO (liftIO), MonadState, StateT)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, withExceptT)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Writer (MonadWriter, WriterT (..), execWriterT, lift, tell)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.ByteString.UTF8 (fromString)
import Data.Data (Data (..), showConstr)
import Data.Either.Combinators (mapLeft)
import Data.Functor ((<&>))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import GTD.Cabal (CabalPackage (..), ModuleNameS, PackageNameS, cabalPackageName, haskellPath)
import GTD.Utils (logDebugNSS, maybeToMaybeT, tryE)
import Language.Haskell.Exts (Decl (..), ExportSpec (..), ExportSpecList (..), ImportDecl (..), ImportSpec (..), ImportSpecList (..), Module (..), ModuleHead (..), ModuleName (..), Name (..), ParseMode (..), ParseResult (..), QName (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, parseFile, parseFileContents, parseFileContentsWithMode, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import Text.Printf (printf)

haskellApplyCppHs :: FilePath -> String -> IO String
haskellApplyCppHs = runCpphs defaultCpphsOptions

haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

identToDecl :: ModuleName SrcSpanInfo -> Name SrcSpanInfo -> Bool -> Declaration
identToDecl m (Symbol l n) = identToDecl' m l n
identToDecl m (Ident l n) = identToDecl' m l n

identToDecl' ::
  ModuleName SrcSpanInfo ->
  SrcSpanInfo ->
  String ->
  Bool ->
  Declaration
identToDecl' (ModuleName _ mn) l n isDeclaration =
  Declaration
    { _declSrcUsage = if isDeclaration then emptySourceSpan else l',
      _declSrcOrig = if isDeclaration then l' else emptySourceSpan,
      _declName = n,
      _declModule = mn
    }
  where
    l' = sourceSpan . srcInfoSpan $ l

haskellGetIdentifiers :: Module SrcSpanInfo -> (MonadWriter [Declaration] m, MonadLogger m, MonadIO m) => m ()
haskellGetIdentifiers m = do
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ _) = h
    forM_ decls $ \case
      TypeSig _ names _ ->
        tell $ (\n -> identToDecl mN n True) <$> names
      _ -> return ()

haskellGetExportedIdentifiers :: Module SrcSpanInfo -> (MonadWriter [Declaration] m, MonadLogger m, MonadIO m) => m Bool
haskellGetExportedIdentifiers m = do
  let logTag = "get exported identifiers"
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \e -> case e of
        EVar _ n -> case n of
          UnQual _ n -> tell [identToDecl mN n False]
          _ -> logDebugNSS logTag $ printf "not yet handled: %s -> %s" (show e) (show n)
        _ -> logDebugNSS logTag $ printf "not yet handled: %s" (show e)
  return $ isJust head

haskellGetImportedIdentifiers :: Module SrcSpanInfo -> (MonadWriter [Declaration] m, MonadLogger m, MonadIO m) => m ()
haskellGetImportedIdentifiers m = do
  let logTag = "get imported identifiers"
  let (Module src head wtf1 imports decls) = m
  forM_ imports $ \(ImportDecl {importModule = im, importSpecs = ss}) -> do
    logDebugNSS logTag $ show im
    case ss of
      Just (ImportSpecList _ isHidden is) -> do
        forM_ is $ \i -> do
          case i of
            IVar _ (Ident l n) -> tell [identToDecl im (Ident l n) False]
            _ -> logDebugNSS logTag $ printf "haskellGetImportedIdentifiers: not yet handled: %s" (show i)
          logDebugNSS logTag $ printf "\t%s" (show i)
      x -> logDebugNSS logTag $ printf ":t = %s" (showConstr . toConstr $ x)

data SourceSpan = SourceSpan
  { sourceSpanFileName :: FilePath,
    sourceSpanStartLine :: Int,
    sourceSpanStartColumn :: Int,
    sourceSpanEndLine :: Int,
    sourceSpanEndColumn :: Int
  }
  deriving (Show, Generic, Eq, Ord)

sourceSpan :: SrcSpan -> SourceSpan
sourceSpan (SrcSpan {srcSpanFilename = fileName, srcSpanStartLine = startLine, srcSpanStartColumn = startColumn, srcSpanEndLine = endLine, srcSpanEndColumn = endColumn}) =
  SourceSpan
    { sourceSpanFileName = fileName,
      sourceSpanStartLine = startLine,
      sourceSpanStartColumn = startColumn,
      sourceSpanEndLine = endLine,
      sourceSpanEndColumn = endColumn
    }

hasNonEmptyOrig :: Declaration -> Bool
hasNonEmptyOrig = (/= emptySourceSpan) . _declSrcOrig

emptySourceSpan :: SourceSpan
emptySourceSpan = SourceSpan "" 0 0 0 0

data Declaration = Declaration
  { _declSrcUsage :: SourceSpan,
    _declSrcOrig :: SourceSpan,
    _declModule :: ModuleNameS,
    _declName :: String
  }
  deriving (Show, Eq, Generic, Ord)

instance FromJSON SourceSpan

instance FromJSON Declaration

instance ToJSON SourceSpan

instance ToJSON Declaration

data Identifier
  = Identifier String
  | QualifiedIdentifier String String
  deriving (Show, Eq, Ord)

data ContextModule = ContextModule
  { _cmodule :: Module SrcSpanInfo,
    _exports :: Map.Map Identifier Declaration,
    _identifiers :: Map.Map Identifier Declaration
  }
  deriving (Show, Eq, Generic)

data ContextCabalPackage = ContextCabalPackage
  { _modules :: Map.Map PackageNameS (Map.Map ModuleNameS ContextModule),
    _dependencies :: [CabalPackage]
  }
  deriving (Show, Generic)

$(makeLenses ''ContextModule)
$(makeLenses ''ContextCabalPackage)

---

parsePackages :: (MonadLogger m, MonadIO m, MonadState ContextCabalPackage m) => m ()
parsePackages = do
  deps <- use dependencies
  forM_ deps $ \dep -> parsePackage dep

parsePackage :: CabalPackage -> (MonadLogger m, MonadIO m, MonadState ContextCabalPackage m) => m ()
parsePackage p = do
  let logTag = "parse package"
  mods <- use modules
  unless ((p ^. cabalPackageName) `Map.member` mods) $ do
    let mods = _cabalPackageExportedModules p
    let root = _cabalPackagePath p
    let srcDirs = _cabalPackageSrcDirs p
    forM_ (Map.assocs mods) $ \(modS, mod) -> do
      forM_ srcDirs $ \srcDir -> do
        r <- runExceptT $ parseModule False (haskellPath root srcDir mod)
        case r of
          Left e -> logDebugNSS logTag $ printf "parseModule failed: %s" e
          Right c2 -> modules %= Map.insertWith Map.union (_cabalPackageName p) (Map.singleton modS c2)

parseModule ::
  Bool ->
  FilePath ->
  (MonadLogger m, MonadIO m, MonadState ContextCabalPackage m) => ExceptT String m ContextModule
parseModule shouldEnrich srcP = do
  let logTag = printf "parsing module %s (enrich=%s)" srcP (show shouldEnrich)
  logDebugNSS logTag $ printf ""

  src <- ExceptT $ mapLeft show <$> (liftIO (try $ readFile srcP) :: (MonadIO m) => m (Either IOException String))
  srcPostCpp <- liftIO $ haskellApplyCppHs srcP src
  mod <- ExceptT $ return $ haskellParse srcP srcPostCpp

  locals <- execWriterT $ haskellGetIdentifiers mod
  importsE <- execWriterT (haskellGetImportedIdentifiers mod) >>= mapM (if shouldEnrich then enrich else return)
  (isImplicitExportAll, exports) <- runWriterT $ haskellGetExportedIdentifiers mod

  logDebugNSS logTag "locals:"
  forM_ locals $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)
  logDebugNSS logTag "importsE:"
  forM_ importsE $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)
  logDebugNSS logTag "exports:"
  forM_ exports $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

  let asDeclsMap ds = Map.fromList $ (\d -> (Identifier $ _declName d, d)) <$> ds
      localsPlusImports = asDeclsMap $ locals ++ importsE
      exportsM = if isImplicitExportAll then asDeclsMap locals else Map.intersection (asDeclsMap exports) localsPlusImports
  return ContextModule {_cmodule = mod, _exports = exportsM, _identifiers = localsPlusImports}

---

enrichTryPackage ::
  Declaration ->
  Map.Map PackageNameS (Map.Map ModuleNameS ContextModule) ->
  CabalPackage ->
  (MonadLogger m) => MaybeT m Declaration
enrichTryPackage d mods dep = do
  guard $ _declModule d `Map.member` _cabalPackageExportedModules dep
  dependencyModules <- maybeToMaybeT $ _cabalPackageName dep `Map.lookup` mods
  enrichTryModule d dependencyModules

enrichTryModule ::
  Declaration ->
  Map.Map ModuleNameS ContextModule ->
  (MonadLogger m) => MaybeT m Declaration
enrichTryModule orig moduleDecls = do
  let logTag = "enrich by module"
  mod <- maybeToMaybeT $ _declModule orig `Map.lookup` moduleDecls
  mDecl <- maybeToMaybeT $ Map.lookup (Identifier $ _declName orig) (_exports mod)
  logDebugNSS logTag $ printf "enrich: updating %s with %s" (show orig) (show mDecl)
  return $ orig {_declSrcOrig = _declSrcOrig mDecl}

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