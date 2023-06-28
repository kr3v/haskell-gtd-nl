{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.AST where

import Control.Lens (makeLenses)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.Writer (MonadWriter (tell), Writer, execWriter, forM_, unless)
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data (toConstr), showConstr)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing, maybeToList)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.Declaration (Declaration (..), Identifier, identToDecl)
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (deduplicate, logDebugNSS)
import Language.Haskell.Exts (ClassDecl (ClsDecl), Decl (..), DeclHead (..), EWildcard (..), ExportSpec (..), ExportSpecList (ExportSpecList), FieldDecl (..), ImportDecl (..), ImportSpec (..), ImportSpecList (ImportSpecList), Language (Haskell2010), Module (Module), ModuleHead (ModuleHead), ModuleHeadAndImports (..), ModuleName (ModuleName), Name (..), NonGreedy (..), ParseMode (..), ParseResult (ParseFailed, ParseOk), Parseable (..), QName (UnQual), QualConDecl (..), SrcSpan (..), SrcSpanInfo (..), defaultParseMode, infix_, parseFileContentsWithMode)
import Language.Haskell.Exts.Extension (Extension (EnableExtension), KnownExtension (FlexibleContexts))
import Language.Haskell.Exts.Syntax (CName (..), ClassDecl, ConDecl (..))
import Text.Printf (printf)

haskellGetImportedSymbols :: [ImportDecl SrcSpanInfo] -> Writer [String] ()
haskellGetImportedSymbols imports =
  forM_ imports $ \(ImportDecl {importQualified = iq, importSrc = isr, importSafe = isa, importPkg = ip, importAs = ia, importSpecs = ss}) -> do
    unless (iq || isr || isa || isJust ia || isJust ip) $ do
      case ss of
        Just (ImportSpecList _ False is) -> forM_ is $ \s -> do
          case s of
            IVar _ (Symbol _ n) -> tell [n]
            _ -> return ()
        _ -> return ()

haskellDropPragmas :: String -> String
haskellDropPragmas c = unlines ((\l -> if not (null l) && head l == '#' then "" else l) <$> lines c)

-- in this case, the proposal is to get all the imported operators and explicitly set a fixity for them
-- though this is the wrong place to do it, since imports might be unspecified
haskellParseAmbigousInfixOperators :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParseAmbigousInfixOperators src content = do
  let ei = parse (haskellDropPragmas content) :: ParseResult (NonGreedy (ModuleHeadAndImports SrcSpanInfo))
  case ei of
    ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)
    ParseOk (NonGreedy (ModuleHeadAndImports _ _ _ is)) -> do
      let operators = execWriter $ haskellGetImportedSymbols is
      let fixs = infix_ 0 operators
      case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010, extensions = [EnableExtension FlexibleContexts], fixities = Just fixs} content of
        ParseOk m -> Right m
        ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

-- TODO: figure out #line pragmas
haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, extensions = [EnableExtension FlexibleContexts], baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> case e of
    "Ambiguous infix expression" -> haskellParseAmbigousInfixOperators src content
    _ -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

---

data ClassOrData = ClassOrData
  { _cdtName :: Declaration,
    _cdtFields :: Map.Map Identifier Declaration,
    _eWildcard :: Bool
  }
  deriving (Show, Generic, Eq)

$(makeLenses ''ClassOrData)

instance FromJSON ClassOrData

instance ToJSON ClassOrData

---

data Declarations = Declarations
  { _decls :: Map.Map Identifier Declaration,
    _dataTypes :: Map.Map Identifier ClassOrData
  }
  deriving (Show, Generic, Eq)

$(makeLenses ''Declarations)

instance Semigroup Declarations where
  (<>) :: Declarations -> Declarations -> Declarations
  (<>) (Declarations d1 dt1) (Declarations d2 dt2) = Declarations (d1 <> d2) (dt1 <> dt2)

instance Monoid Declarations where
  mempty :: Declarations
  mempty = Declarations mempty mempty

instance FromJSON Declarations

instance ToJSON Declarations

qualConDeclAsDataC :: ModuleName SrcSpanInfo -> QualConDecl SrcSpanInfo -> [Declaration]
qualConDeclAsDataC mN (QualConDecl _ _ _ cd) =
  let (n, fs) = case cd of
        ConDecl _ n _ -> (n, [])
        InfixConDecl _ _ n _ -> (n, [])
        RecDecl _ n fs -> (n,) $ concatMap (\(FieldDecl _ ns _) -> ns) fs
   in [identToDecl mN n True] <> (flip (identToDecl mN) True <$> fs)

classDeclAsDecls :: ModuleName SrcSpanInfo -> ClassDecl SrcSpanInfo -> [Declaration]
classDeclAsDecls mN (ClsDecl _ (TypeSig _ names _)) = (\n -> identToDecl mN n True) <$> names
classDeclAsDecls _ _ = []

haskellGetIdentifierFromDeclHead :: DeclHead SrcSpanInfo -> Maybe (Name SrcSpanInfo)
haskellGetIdentifierFromDeclHead (DHead _ n) = Just n
haskellGetIdentifierFromDeclHead (DHParen _ h) = haskellGetIdentifierFromDeclHead h
-- FIXME: I don't know if `haskellGetIdentifierFromDeclHead (DHApp _ h _)` handler is correct
haskellGetIdentifierFromDeclHead (DHApp _ h _) = haskellGetIdentifierFromDeclHead h
haskellGetIdentifierFromDeclHead _ = Nothing

haskellGetIdentifiers :: Module SrcSpanInfo -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
haskellGetIdentifiers (Module l mhead _ _ ds) = do
  let logTag = "get identifiers @ Module (" ++ (srcSpanFilename . srcInfoSpan $ l) ++ ")"
  forM_ mhead $ \h -> do
    let (ModuleHead _ mN _ _) = h
    forM_ ds $ \case
      TypeSig _ names _ -> tell $ mempty {_decls = asDeclsMap $ (\n -> identToDecl mN n True) <$> names}
      DataDecl _ _ _ dh qcds _ -> do
        case haskellGetIdentifierFromDeclHead dh of
          Just n -> do
            let d = identToDecl mN n True
            let ctors = concatMap (qualConDeclAsDataC mN) qcds
            tell $ mempty {_dataTypes = Map.singleton (_declName d) $ ClassOrData d (asDeclsMap ctors) False}
          Nothing -> logDebugNSS logTag (printf "not yet handled: DataDecl :t dh = %s" (showConstr . toConstr $ dh))
      ClassDecl _ _ dh _ mcds -> do
        case haskellGetIdentifierFromDeclHead dh of
          Just n -> do
            let d = identToDecl mN n True
            tell $ mempty {_dataTypes = Map.singleton (_declName d) $ ClassOrData d (asDeclsMap $ concatMap (classDeclAsDecls mN) (concat $ maybeToList mcds)) False}
          Nothing -> logDebugNSS logTag (printf "not yet handled: ClassDecl :t dh = %s" (showConstr . toConstr $ dh))
      TypeDecl _ dh _ -> forM_ (haskellGetIdentifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
      TypeFamDecl _ dh _ _ -> forM_ (haskellGetIdentifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
      ClosedTypeFamDecl _ dh _ _ _ -> forM_ (haskellGetIdentifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
      GDataDecl _ _ _ dh _ _ _ -> forM_ (haskellGetIdentifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
      DataFamDecl _ _ dh _ -> forM_ (haskellGetIdentifierFromDeclHead dh) $ \n -> tell $ mempty {_decls = asDeclsMap [identToDecl mN n True]}
      c -> logDebugNSS logTag (printf "not yet handled: :t c = %s" (showConstr . toConstr $ c))
haskellGetIdentifiers m = logDebugNSS "get identifiers" (printf "not yet handled: :t m = %s" (showConstr . toConstr $ m))

---

data Exports = Exports
  { exportedVars :: [Declaration],
    exportedModules :: [ModuleNameS],
    exportedCDs :: [ClassOrData]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Exports

instance ToJSON Exports

instance Semigroup Exports where
  (<>) :: Exports -> Exports -> Exports
  (<>) (Exports es1 rs1 ed1) (Exports es2 rs2 ed2) = Exports (es1 <> es2) (rs1 <> rs2) (ed1 <> ed2)

instance Monoid Exports where
  mempty :: Exports
  mempty = Exports [] [] []

handleCName :: CName l -> Name l
handleCName (VarName _ n) = n
handleCName (ConName _ n) = n

isEWildcard :: EWildcard l -> Bool
isEWildcard (NoWildcard _) = False
isEWildcard (EWildcard _ _) = True

classOrDataE :: ModuleName SrcSpanInfo -> Name SrcSpanInfo -> [CName SrcSpanInfo] -> Bool -> ClassOrData
classOrDataE mN n cs = ClassOrData (identToDecl mN n False) $ asDeclsMap (flip (identToDecl mN) False . handleCName <$> cs)

haskellGetExports :: Module SrcSpanInfo -> (MonadWriter Exports m, MonadLoggerIO m) => m Bool
haskellGetExports (Module _ mhead _ _ _) = do
  forM_ mhead $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \e -> case e of
        EModuleContents _ (ModuleName _ mn) -> tell mempty {exportedModules = [mn]}
        EVar _ (UnQual _ n) -> tell mempty {exportedVars = [identToDecl mN n False]}
        EVar _ qn -> logDebugNSS "get exports" $ printf "not yet handled: %s -> %s" (show e) (show qn)
        EAbs _ _ (UnQual _ n) -> tell mempty {exportedCDs = [classOrDataE mN n [] False]}
        EAbs _ _ qn -> logDebugNSS "get exports" $ printf "not yet handled: %s -> %s" (show e) (show qn)
        EThingWith _ w qn cs -> case qn of
          UnQual _ qn' -> tell mempty {exportedCDs = [classOrDataE mN qn' cs (isEWildcard w)]}
          _ -> logDebugNSS "get exports" $ printf "not yet handled: %s -> %s" (show e) (show qn)
  return $ isNothing mhead
haskellGetExports m = logDebugNSS "get exports" (printf "not yet handled: :t m = %s" (showConstr . toConstr $ m)) >> return False

---

data Imports = Imports
  { importedDecls :: [Declaration],
    importedModules :: [ModuleNameS],
    importedCDs :: [ClassOrData]
  }
  deriving (Show, Generic, Eq)

instance Semigroup Imports where
  (<>) :: Imports -> Imports -> Imports
  (<>) (Imports is1 ims1 icd1) (Imports is2 ims2 icd2) = Imports (is1 <> is2) (ims1 <> ims2) (icd1 <> icd2)

instance Monoid Imports where
  mempty :: Imports
  mempty = Imports [] [] []

allImportedModules :: Imports -> [ModuleNameS]
allImportedModules (Imports ids ims icds) = deduplicate $ ims <> (_declModule <$> ids) <> (_declModule . _cdtName <$> icds)

haskellGetImports :: Module SrcSpanInfo -> (MonadWriter Imports m, MonadLoggerIO m) => m ()
haskellGetImports (Module _ _ _ is _) = do
  let logTag = "get imports"
  forM_ is $ \(ImportDecl {importModule = im@(ModuleName _ imn), importQualified = iq, importSrc = isr, importSafe = isa, importPkg = ip, importAs = ia, importSpecs = ss}) -> do
    unless (iq || isr || isa || isJust ia || isJust ip) $ do
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
        Nothing -> tell mempty {importedModules = [imn]}
haskellGetImports m = logDebugNSS "get imports" $ printf "not yet handled: :t m = %s" (showConstr . toConstr $ m)
