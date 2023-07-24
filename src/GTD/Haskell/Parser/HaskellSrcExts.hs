{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.Parser.HaskellSrcExts where

import Control.Lens (makeLenses)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter, forM_, unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data (toConstr), showConstr)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, maybeToList)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.Declaration (ClassOrData (ClassOrData), Declaration (_declName), Declarations (_dataTypes, _decls), Exports (exportedCDs, exportedModules, exportedVars), Imports (importedCDs, importedDecls, importedModules), asDeclsMap, identToDecl, name)
import qualified GTD.Haskell.Declaration as Declarations
import GTD.Utils (deduplicate, logDebugNSS)
import Language.Haskell.Exts (ClassDecl (ClsDecl), Decl (..), DeclHead (..), EWildcard (..), ExportSpec (..), ExportSpecList (ExportSpecList), FieldDecl (..), ImportDecl (..), ImportSpec (..), ImportSpecList (ImportSpecList), KnownExtension (..), Language (Haskell2010), Module (Module), ModuleHead (ModuleHead), ModuleHeadAndImports (..), ModuleName (ModuleName), ModulePragma (..), Name (..), NonGreedy (..), ParseMode (..), ParseResult (ParseFailed, ParseOk), Parseable (..), QName (UnQual), QualConDecl (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, infix_, parseFileContentsWithMode)
import qualified Language.Haskell.Exts as Exts
import Language.Haskell.Exts.Extension (Extension (EnableExtension))
import Language.Haskell.Exts.Syntax (CName (..), ConDecl (..))
import Text.Printf (printf)

class HsParser a

importedSymbols :: [ImportDecl SrcSpanInfo] -> Writer [String] ()
importedSymbols imports =
  forM_ imports $ \(ImportDecl {importQualified = iq, importSrc = isr, importSafe = isa, importPkg = ip, importAs = ia, importSpecs = ss}) -> unless (iq || isr || isa || isJust ia || isJust ip) $ do
    case ss of
      Just (ImportSpecList _ False is) -> forM_ is $ \s -> do
        case s of
          IVar _ (Symbol _ n) -> tell [n]
          _ -> return ()
      _ -> return ()

dropPragmas :: String -> String
dropPragmas c = unlines ((\l -> if not (null l) && head l == '#' then "" else l) <$> lines c)

-- in this case, the proposal is to get all the imported operators and explicitly set a fixity for them
-- though this is the wrong place to do it, since imports might be unspecified
parseAmbigousInfixOperators :: FilePath -> String -> Either String (Module SrcSpanInfo)
parseAmbigousInfixOperators src content = do
  let ei = Exts.parse (dropPragmas content) :: ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo))
  case ei of
    ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src (take 512 e) (show loc)
    ParseOk (NonGreedy (ModuleHeadAndImports _ _ _ is)) -> do
      let operators = execWriter $ importedSymbols is
      let fixs = infix_ 0 operators
      case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010, extensions = [EnableExtension FlexibleContexts, EnableExtension StandaloneDeriving], fixities = Just fixs} content of
        ParseOk m -> Right m
        ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src (take 512 e) (show loc)

-- TODO: figure out #line pragmas
parse :: FilePath -> String -> Either String (Module SrcSpanInfo)
parse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, extensions = [EnableExtension FlexibleContexts], baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> case e of
    "Ambiguous infix expression" -> parseAmbigousInfixOperators src content
    _ -> Left $ printf "failed to parse %s: %s @ %s" src (take 512 e) (show loc)

---

qualConDeclAsDataC' :: QualConDecl a -> (Name a, [Name a])
qualConDeclAsDataC' (QualConDecl _ _ _ (ConDecl _ n ts)) = (n, [])
qualConDeclAsDataC' (QualConDecl _ _ _ (InfixConDecl _ _ n _)) = (n, [])
qualConDeclAsDataC' (QualConDecl _ _ _ (RecDecl _ n fs)) = (n, concatMap (\(FieldDecl _ ns _) -> ns) fs)

qualConDeclAsDataC :: ModuleName SrcSpanInfo -> QualConDecl SrcSpanInfo -> [Declaration]
qualConDeclAsDataC mN qc =
  let (n, fs) = qualConDeclAsDataC' qc
   in [identToDecl mN n True] <> (flip (identToDecl mN) True <$> fs)

classDeclAsDecls :: ModuleName SrcSpanInfo -> ClassDecl SrcSpanInfo -> [Declaration]
classDeclAsDecls mN (ClsDecl _ (TypeSig _ names _)) = (\n -> identToDecl mN n True) <$> names
classDeclAsDecls _ _ = []

identifierFromDeclHead :: DeclHead SrcSpanInfo -> Maybe (Name SrcSpanInfo)
identifierFromDeclHead (DHead _ n) = Just n
identifierFromDeclHead (DHParen _ h) = identifierFromDeclHead h
-- FIXME: I don't know if `identifierFromDeclHead (DHApp _ h _)` handler is correct
identifierFromDeclHead (DHApp _ h _) = identifierFromDeclHead h
identifierFromDeclHead _ = Nothing

identifiers :: Module SrcSpanInfo -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
identifiers (Module l (Just (ModuleHead _ mN _ _)) _ _ ds) = do
  let logTag = printf "get identifiers @ Module (%s)" (srcSpanFilename $ srcInfoSpan l)
  forM_ ds $ \case
    TypeSig _ names _ -> tell $ mempty {_decls = asDeclsMap $ (\n -> identToDecl mN n True) <$> names}
    DataDecl _ _ _ dh qcds _ -> do
      case identifierFromDeclHead dh of
        Just n -> do
          let d = identToDecl mN n True
          let ctors = concatMap (qualConDeclAsDataC mN) qcds
          tell $ mempty {_dataTypes = Map.singleton (_declName d) $ ClassOrData d (asDeclsMap ctors) False}
        Nothing -> logDebugNSS logTag (printf "not yet handled: DataDecl :t dh = %s" (showConstr . toConstr $ dh))
    ClassDecl _ _ dh _ mcds -> do
      case identifierFromDeclHead dh of
        Just n -> do
          let d = identToDecl mN n True
          tell $ mempty {_dataTypes = Map.singleton (_declName d) $ ClassOrData d (asDeclsMap $ concatMap (classDeclAsDecls mN) (concat $ maybeToList mcds)) False}
        Nothing -> logDebugNSS logTag (printf "not yet handled: ClassDecl :t dh = %s" (showConstr . toConstr $ dh))
    TypeDecl _ dh _ -> forM_ (identifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
    TypeFamDecl _ dh _ _ -> forM_ (identifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
    ClosedTypeFamDecl _ dh _ _ _ -> forM_ (identifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
    GDataDecl _ _ _ dh _ _ _ -> forM_ (identifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
    DataFamDecl _ _ dh _ -> forM_ (identifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
    c -> logDebugNSS logTag (printf "not yet handled: :t c = %s" (showConstr . toConstr $ c))
identifiers (Module _ Nothing _ _ ds) = return ()
identifiers m = logDebugNSS "get identifiers" (printf "not yet handled: :t m = %s" (showConstr . toConstr $ m))

---

handleCName :: CName l -> Name l
handleCName (VarName _ n) = n
handleCName (ConName _ n) = n

isEWildcard :: EWildcard l -> Bool
isEWildcard (NoWildcard _) = False
isEWildcard (EWildcard _ _) = True

classOrDataE :: ModuleName SrcSpanInfo -> Name SrcSpanInfo -> [CName SrcSpanInfo] -> Bool -> ClassOrData
classOrDataE mN n cs = ClassOrData (identToDecl mN n False) $ asDeclsMap (flip (identToDecl mN) False . handleCName <$> cs)

-- returns True if the module implicitly exports all its identifiers
exports :: Module SrcSpanInfo -> (MonadWriter Exports m, MonadLoggerIO m) => m Bool
exports (Module _ (Just (ModuleHead _ mN _ (Just (ExportSpecList _ es)))) _ _ _) = do
  forM_ es $ \e -> case e of
    EModuleContents _ (ModuleName _ mn) -> tell mempty {exportedModules = [mn]}
    EVar _ (UnQual _ n) -> tell mempty {exportedVars = [identToDecl mN n False]}
    EVar _ qn -> logDebugNSS "get exports" $ printf "not yet handled: %s -> %s" (show e) (show qn)
    EAbs _ _ (UnQual _ n) -> tell mempty {exportedCDs = [classOrDataE mN n [] False]}
    EAbs _ _ qn -> logDebugNSS "get exports" $ printf "not yet handled: %s -> %s" (show e) (show qn)
    EThingWith _ w qn cs -> case qn of
      UnQual _ qn' -> tell mempty {exportedCDs = [classOrDataE mN qn' cs (isEWildcard w)]}
      _ -> logDebugNSS "get exports" $ printf "not yet handled: %s -> %s" (show e) (show qn)
  return False
exports (Module _ Nothing _ _ _) = return True
exports (Module _ (Just (ModuleHead _ _ _ Nothing)) _ _ _) = return True
exports m = logDebugNSS "get exports" (printf "not yet handled: :t m = %s" (showConstr . toConstr $ m)) >> return False

---

importsE :: Module SrcSpanInfo -> (MonadWriter Imports m, MonadLoggerIO m) => m ()
importsE (Module _ _ _ is _) = do
  let logTag = "get imports (e)"
  forM_ is $ \(ImportDecl {importModule = im@(ModuleName _ imn), importQualified = iq, importSrc = isr, importSafe = isa, importPkg = ip, importAs = ia, importSpecs = ss}) -> unless (iq || isr || isa || isJust ia || isJust ip) $ do
    logDebugNSS logTag $ printf "handling module=%s (:t(ss) == %s)" (show im) (showConstr $ toConstr ss)
    case ss of
      Just (ImportSpecList _ False is') -> do
        forM_ is' $ \i -> do
          case i of
            IVar _ n -> tell mempty {importedDecls = [identToDecl im n False]}
            IAbs _ _ n -> tell mempty {importedCDs = [classOrDataE im n [] False]}
            IThingAll _ n -> tell mempty {importedCDs = [classOrDataE im n [] True]}
            IThingWith _ n cs -> tell mempty {importedCDs = [classOrDataE im n cs False]}
      Just (ImportSpecList _ True _) -> logDebugNSS logTag $ printf "not yet handled: isHidden is `True`"
      Nothing -> tell mempty {importedModules = [mempty {Declarations._modName = imn}]}
importsE m = logDebugNSS "get imports" $ printf "not yet handled: :t m = %s" (showConstr . toConstr $ m)

isNoImplicitPrelude :: Module SrcSpanInfo -> Bool
isNoImplicitPrelude (Module _ _ ps _ _) = flip any ps $ \case
  LanguagePragma _ ns -> any (\n -> name n == "NoImplicitPrelude") ns
  _ -> False

importsP :: Module SrcSpanInfo -> (MonadWriter Imports m, MonadLoggerIO m) => m ()
importsP m = unless (isNoImplicitPrelude m) $ do
  tell mempty {importedModules = [mempty {Declarations._modName = "Prelude"}]}

imports :: Module SrcSpanInfo -> (MonadWriter Imports m, MonadLoggerIO m) => m ()
imports m = do
  importsE m
  importsP m
