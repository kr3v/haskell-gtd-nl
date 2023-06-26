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
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import GTD.Haskell.Declaration (Declaration (..), Identifier, identToDecl)
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (deduplicate, logDebugNSS)
import Language.Haskell.Exts (Decl (..), DeclHead (..), EWildcard (..), ExportSpec (..), ExportSpecList (ExportSpecList), FieldDecl (..), ImportDecl (..), ImportSpec (..), ImportSpecList (ImportSpecList), Language (Haskell2010), Module (Module), ModuleHead (ModuleHead), ModuleHeadAndImports (..), ModuleName (ModuleName), Name (..), NonGreedy (..), ParseMode (..), ParseResult (ParseFailed, ParseOk), Parseable (..), QName (UnQual), QualConDecl (..), SrcSpanInfo, defaultParseMode, infix_, parseFileContentsWithMode)
import Language.Haskell.Exts.Syntax (CName (..), ConDecl (..))
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
      case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010, fixities = Just fixs} content of
        ParseOk m -> Right m
        ParseFailed loc e -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

-- TODO: figure out #line pragmas
haskellParse :: FilePath -> String -> Either String (Module SrcSpanInfo)
haskellParse src content = case parseFileContentsWithMode defaultParseMode {parseFilename = src, baseLanguage = Haskell2010} content of
  ParseOk m -> Right m
  ParseFailed loc e -> case e of
    "Ambiguous infix expression" -> haskellParseAmbigousInfixOperators src content
    _ -> Left $ printf "failed to parse %s: %s @ %s" src e (show loc)

haskellGetIdentifierFromDeclHead :: DeclHead SrcSpanInfo -> Maybe (Name SrcSpanInfo)
haskellGetIdentifierFromDeclHead (DHead _ n) = Just n
haskellGetIdentifierFromDeclHead (DHParen _ h) = haskellGetIdentifierFromDeclHead h
haskellGetIdentifierFromDeclHead _ = Nothing

data DataD = DataD
  { dataName :: Declaration,
    dataCtors :: Map.Map Identifier DataDC
  }
  deriving (Show, Generic, Eq)

data DataDC = DataDC
  { constructorName :: Declaration,
    constructorFields :: Map.Map Identifier Declaration
  }
  deriving (Show, Generic, Eq)

instance FromJSON DataD

instance ToJSON DataD

instance FromJSON DataDC

instance ToJSON DataDC

data Declarations = Declarations
  { _decls :: Map.Map Identifier Declaration,
    _dataTypes :: Map.Map Identifier DataD
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

qualConDeclAsDataC :: ModuleName SrcSpanInfo -> QualConDecl SrcSpanInfo -> DataDC
qualConDeclAsDataC mN (QualConDecl _ _ _ cd) =
  let (n, fs) = case cd of
        ConDecl _ n _ -> (n, [])
        InfixConDecl _ _ n _ -> (n, [])
        RecDecl _ n fs -> (n,) $ concatMap (\(FieldDecl _ ns _) -> ns) fs
   in DataDC
        { constructorName = identToDecl mN n True,
          constructorFields = asDeclsMap $ flip (identToDecl mN) True <$> fs
        }

haskellGetIdentifiers :: Module SrcSpanInfo -> (MonadWriter Declarations m, MonadLoggerIO m) => m ()
haskellGetIdentifiers (Module _ mhead _ _ decls) =
  forM_ mhead $ \h -> do
    let (ModuleHead _ mN _ _) = h
    forM_ decls $ \case
      TypeSig _ names _ ->
        tell $ mempty {_decls = asDeclsMap $ (\n -> identToDecl mN n True) <$> names}
      DataDecl _ _ _ dh qcds _ -> do
        forM_ (flip (identToDecl mN) True <$> haskellGetIdentifierFromDeclHead dh) $ \d -> do
          let ctors = qualConDeclAsDataC mN <$> qcds
          tell $ mempty {_dataTypes = Map.singleton (_declName d) $ DataD d $ Map.fromList $ (\c -> (_declName . constructorName $ c, c)) <$> ctors}
      _ -> return ()
haskellGetIdentifiers m = logDebugNSS "get identifiers" (printf "not yet handled: :t m = %s" (showConstr . toConstr $ m))

---

data ClassOrData = ClassOrData
  { eName :: Declaration,
    eExports :: [Declaration],
    eWildcard :: Bool
  }
  deriving (Show, Generic, Eq)

instance FromJSON ClassOrData

instance ToJSON ClassOrData

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

handleQName :: (Show a) => a -> ModuleName SrcSpanInfo -> QName SrcSpanInfo -> (MonadWriter Exports m, MonadLoggerIO m) => m ()
handleQName _ mN (UnQual _ n) = tell mempty {exportedVars = [identToDecl mN n False]}
handleQName r _ qn = logDebugNSS "get exports" $ printf "not yet handled: %s -> %s" (show r) (show qn)

handleCName :: CName l -> Name l
handleCName (VarName _ n) = n
handleCName (ConName _ n) = n

isEWildcard :: EWildcard l -> Bool
isEWildcard (NoWildcard _) = False
isEWildcard (EWildcard _ _) = True

classOrDataE :: ModuleName SrcSpanInfo -> Name SrcSpanInfo -> [CName SrcSpanInfo] -> Bool -> ClassOrData
classOrDataE mN n cs = ClassOrData (identToDecl mN n False) (flip (identToDecl mN) False . handleCName <$> cs)

haskellGetExports :: Module SrcSpanInfo -> (MonadWriter Exports m, MonadLoggerIO m) => m Bool
haskellGetExports (Module _ mhead _ _ _) = do
  forM_ mhead $ \h -> do
    let (ModuleHead _ mN _ mE) = h
    forM_ mE $ \(ExportSpecList _ es) ->
      forM_ es $ \e -> case e of
        EVar _ qn -> handleQName e mN qn
        EAbs _ _ qn -> handleQName e mN qn
        EModuleContents _ (ModuleName _ mn) -> tell mempty {exportedModules = [mn]}
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
allImportedModules (Imports ids ims icds) = deduplicate $ ims <> (_declModule <$> ids) <> (_declModule . eName <$> icds)

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
              IAbs _ _ n -> tell mempty {importedDecls = [identToDecl im n False]}
              IThingAll _ n -> tell mempty {importedCDs = [classOrDataE im n [] True]}
              IThingWith _ n cs -> tell mempty {importedCDs = [classOrDataE im n cs False]}
        Just (ImportSpecList _ True _) -> logDebugNSS logTag $ printf "not yet handled: isHidden is `True`"
        Nothing -> tell mempty {importedModules = [imn]}
haskellGetImports m = logDebugNSS "get imports" $ printf "not yet handled: :t m = %s" (showConstr . toConstr $ m)
