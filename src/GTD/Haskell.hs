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
import Control.Monad.Logger (MonadLogger, logDebugN, logDebugNS, logDebugSH)
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
import GTD.Utils (logDebugNSS, logErrorNSS, maybeToMaybeT, tryE, withExceptT)
import Language.Haskell.Exts (Decl (..), ExportSpec (..), ExportSpecList (..), ImportDecl (..), ImportSpec (..), ImportSpecList (..), Module (..), ModuleHead (..), ModuleName (..), Name (..), ParseMode (..), ParseResult (..), QName (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, parseFile, parseFileContents, parseFileContentsWithMode, parseFileWithCommentsAndPragmas, prettyPrint)
import Language.Haskell.Exts.Extension (Language (..))
import Language.Haskell.Exts.SrcLoc (SrcSpanInfo)
import Language.Preprocessor.Cpphs (defaultCpphsOptions, runCpphs)
import Text.Printf (printf)

haskellApplyCppHs :: MonadIO m => FilePath -> String -> ExceptT String m String
haskellApplyCppHs srcP srcC = do
  e <- liftIO $ tryAny (runCpphs defaultCpphsOptions srcP srcC)
  case e of
    Left e -> throwE $ printf "failed to apply cpphs to %s" srcP
    Right r -> return r

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

haskellGetReexportedModules :: Module SrcSpanInfo -> (MonadWriter [ModuleNameS] m, MonadLogger m, MonadIO m) => m ()
haskellGetReexportedModules m = do
  let logTag = "get re-exported modules"
  let (Module src head wtf1 imports decls) = m
  forM_ head $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \case
        EModuleContents _ (ModuleName _ n) -> tell [n]
        _ -> return ()

haskellGetImportedModules :: Module SrcSpanInfo -> (MonadWriter [ModuleNameS] m, MonadLogger m, MonadIO m) => m ()
haskellGetImportedModules m = do
  let logTag = "get imported modules"
  let (Module src head wtf1 imports decls) = m
  forM_ imports $ \(ImportDecl {importModule = (ModuleName _ mn), importSpecs = ss}) -> do
    tell [mn]

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
  { _cpackage :: PackageNameS,
    _cmoduleName :: ModuleNameS,
    _ccmodule :: Cabal.ModuleName,
    _cmodulePath :: FilePath,
    _cmodule :: Module SrcSpanInfo,
    _cdependencies :: [ModuleNameS],
    _exports :: Map.Map Identifier Declaration,
    _locals :: Map.Map Identifier Declaration,
    _identifiers :: Map.Map Identifier Declaration
  }
  deriving (Show, Eq, Generic)

emptySrcSpan :: SrcSpan
emptySrcSpan = SrcSpan {srcSpanFilename = "", srcSpanStartLine = 0, srcSpanStartColumn = 0, srcSpanEndLine = 0, srcSpanEndColumn = 0}

emptySrcSpanInfo :: SrcSpanInfo
emptySrcSpanInfo = SrcSpanInfo {srcInfoSpan = emptySrcSpan, srcInfoPoints = []}

emptyHaskellModule :: Module SrcSpanInfo
emptyHaskellModule = Module emptySrcSpanInfo Nothing [] [] []

emptyContextModule :: ContextModule
emptyContextModule =
  ContextModule
    { _cpackage = "",
      _cmoduleName = "",
      _cmodulePath = "",
      _ccmodule = Cabal.main,
      _cmodule = emptyHaskellModule,
      _cdependencies = [],
      _exports = Map.empty,
      _locals = Map.empty,
      _identifiers = Map.empty
    }

data ContextCabalPackage = ContextCabalPackage
  { _modules :: Map.Map PackageNameS (Map.Map ModuleNameS ContextModule),
    _dependencies :: [CabalPackage]
  }
  deriving (Show, Generic)

$(makeLenses ''ContextModule)
$(makeLenses ''ContextCabalPackage)

---

asDeclsMap :: [Declaration] -> Map.Map Identifier Declaration
asDeclsMap ds = Map.fromList $ (\d -> (Identifier $ _declName d, d)) <$> ds

parseModulePhase1 ::
  ContextModule ->
  (MonadLogger m, MonadIO m) => ExceptT String m ContextModule
parseModulePhase1 cm = do
  let srcP = _cmodulePath cm
  let logTag = printf "parsing module %s" srcP
  logDebugNSS logTag $ printf ""

  src <- ExceptT $ mapLeft show <$> (liftIO (try $ readFile srcP) :: (MonadIO m) => m (Either IOException String))
  srcPostCpp <- haskellApplyCppHs srcP src
  mod <- ExceptT $ return $ haskellParse srcP srcPostCpp

  locals <- execWriterT $ haskellGetIdentifiers mod
  logDebugNSS logTag "locals:"
  forM_ locals $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

  return $ cm {_cmodule = mod, _locals = asDeclsMap locals}

parseModule0 ::
  CabalPackage ->
  ModuleNameS ->
  Cabal.ModuleName ->
  (MonadLogger m, MonadIO m) => m [Either (CabalLibSrcDir, ModuleNameS, String) ContextModule]
parseModule0 p modS mod = do
  let mods = _cabalPackageExportedModules p
  let root = _cabalPackagePath p
  let srcDirs = _cabalPackageSrcDirs p
  forM srcDirs $ \srcDir -> runExceptT $ do
    let path = haskellPath root srcDir mod
    let cm = emptyContextModule {_cpackage = p ^. cabalPackageName, _cmoduleName = modS, _cmodulePath = path, _ccmodule = mod}
    r <- withExceptT (srcDir,modS,) (parseModulePhase1 cm)
    eM <- lift $ execWriterT $ haskellGetReexportedModules (_cmodule r)
    iM <- lift $ execWriterT $ haskellGetImportedModules (_cmodule r)
    return r {_cdependencies = eM ++ iM}

parseModule1 ::
  CabalPackage ->
  ModuleNameS ->
  Cabal.ModuleName ->
  (MonadLogger m, MonadIO m) => m ([String], Maybe ContextModule)
parseModule1 p modS mod = do
  es <- parseModule0 p modS mod

  let (errors, modules) = partitionEithers es
  let errorsS = (\(srcDir, modS, e) -> printf "error parsing module %s/%s: %s" (show srcDir) (show modS) (show e)) <$> errors

  return $ case length modules of
    0 -> (errorsS, Nothing)
    1 -> (errorsS, Just $ head modules)
    _ -> (errorsS ++ [printf "multiple modules found: %s" (show modules)], Nothing)

parseModule ::
  CabalPackage ->
  ModuleNameS ->
  Cabal.ModuleName ->
  (MonadLogger m, MonadIO m) => m (Maybe ContextModule)
parseModule p modS mod = do
  let logTag = "parse module - p1"
  (es, cm) <- parseModule1 p modS mod
  forM_ es (logErrorNSS logTag)
  return cm

---

data MS = MS
  { _msModules :: Map.Map ModuleNameS ContextModule,
    _msFailedModules :: Set.Set ModuleNameS
  }
  deriving (Show, Generic)

$(makeLenses ''MS)

parseChosenModules0 ::
  CabalPackage ->
  Map.Map ModuleNameS Cabal.ModuleName ->
  (MonadLogger m, MonadIO m, MonadState MS m) => m (Set.Set ModuleNameS)
parseChosenModules0 p mods = do
  let root = _cabalPackagePath p
  let srcDirs = _cabalPackageSrcDirs p

  modulesE <- forM (Map.assocs mods) $ \(modS, mod) -> do
    cMod <- parseModule p modS mod
    return $ case cMod of
      Nothing -> Left modS
      Just m -> Right m
  let (failedModulesN', modules) = partitionEithers modulesE

  let failedModulesN = Set.fromList failedModulesN'
  failedModulesO <- use msFailedModules
  let failedModules = failedModulesN <> failedModulesO

  let modulesN = Map.fromList $ (\m -> (_cmoduleName m, m)) <$> modules
  modulesO <- use msModules
  let mModules = modulesN <> modulesO

  let missingModules = Set.fromList (concatMap _cdependencies modules) `Set.difference` Map.keysSet mModules
  let missingNonFailedModules = missingModules `Set.difference` failedModules

  msModules %= Map.union mModules
  msFailedModules .= failedModules
  return missingNonFailedModules

parseChosenModules ::
  CabalPackage ->
  Map.Map ModuleNameS Cabal.ModuleName ->
  (MonadLogger m, MonadIO m, MonadState MS m) => m (Set.Set ModuleNameS)
parseChosenModules p mods = do
  let logTag = "parse given set of modules"
  missingModules <- parseChosenModules0 p mods

  logDebugNSS logTag $ printf "missing modules: %s" (show missingModules)
  modules <- use msModules
  logDebugNSS logTag $ printf "modules: %s" (show (Map.keysSet modules))
  failedModules <- use msFailedModules
  logDebugNSS logTag $ printf "failed modules: %s" (show failedModules)

  if Set.null missingModules
    then return Set.empty
    else parseChosenModules p (Map.fromList $ (\x -> (x, Cabal.fromString x)) <$> Set.toList missingModules)

parsePackages :: (MonadLogger m, MonadIO m, MonadState ContextCabalPackage m) => m ()
parsePackages = do
  deps <- use dependencies
  forM_ deps $ \dep -> parsePackage dep

parsePackage ::
  CabalPackage ->
  (MonadLogger m, MonadIO m) => m (Map.Map ModuleNameS ContextModule)
parsePackage p = do
  let logTag = "parse package"
  let ms = MS Map.empty Set.empty
  ms' <- execStateT (parseChosenModules p (_cabalPackageExportedModules p)) ms
  return $ _msModules ms'

---

parseModulePhase2 ::
  Bool ->
  FilePath ->
  ContextModule ->
  (MonadLogger m, MonadIO m, MonadState ContextCabalPackage m) => ExceptT String m ContextModule
parseModulePhase2 shouldEnrich srcP cm = do
  let logTag = printf "parsing module %s (enrich=%s)" srcP (show shouldEnrich)
  logDebugNSS logTag $ printf ""

  imports <- execWriterT (haskellGetImportedIdentifiers (_cmodule cm)) >>= mapM (if shouldEnrich then enrich else return)

  logDebugNSS logTag "imports:"
  forM_ imports $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

  return $ cm {_identifiers = Map.union (_locals cm) (asDeclsMap imports)}

parseModulePhase3 ::
  FilePath ->
  ContextModule ->
  (MonadLogger m, MonadIO m, MonadState ContextCabalPackage m) => ExceptT String m ContextModule
parseModulePhase3 srcP cm = do
  let logTag = printf "parsing module 2 - %s" srcP

  logDebugNSS logTag "exports:"
  (isImplicitExportAll, exports) <- runWriterT $ haskellGetExportedIdentifiers (_cmodule cm)
  forM_ exports $ \i -> logDebugNSS logTag $ printf "\t%s" (show i)

  return $ cm {_exports = if isImplicitExportAll then _locals cm else Map.intersection (asDeclsMap exports) (_identifiers cm)}

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