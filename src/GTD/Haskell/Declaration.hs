{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Haskell.Declaration where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import GTD.Utils (deduplicate)
import Language.Haskell.Exts (ModuleName (..), Name (..), SrcSpan (..), SrcSpanInfo (srcInfoSpan))

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

emptySourceSpan :: SourceSpan
emptySourceSpan = SourceSpan "" 0 0 0 0

instance FromJSON SourceSpan

instance ToJSON SourceSpan

---

data Declaration = Declaration
  { _declSrcOrig :: SourceSpan,
    _declModule :: ModuleNameS,
    _declName :: String
  }
  deriving (Show, Eq, Generic, Ord)

$(makeLenses ''Declaration)

instance FromJSON Declaration

instance ToJSON Declaration

hasNonEmptyOrig :: Declaration -> Bool
hasNonEmptyOrig = (/= emptySourceSpan) . _declSrcOrig

---

type Identifier = String

name :: Name a -> String
name (Ident _ n) = n
name (Symbol _ n) = n

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
    { _declSrcOrig = if isDeclaration then l' else emptySourceSpan,
      _declName = n,
      _declModule = mn
    }
  where
    l' = sourceSpan . srcInfoSpan $ l

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

instance FromJSON Declarations

instance ToJSON Declarations

instance Semigroup Declarations where
  (<>) :: Declarations -> Declarations -> Declarations
  (<>) (Declarations d1 dt1) (Declarations d2 dt2) = Declarations (d1 <> d2) (dt1 <> dt2)

instance Monoid Declarations where
  mempty :: Declarations
  mempty = Declarations mempty mempty

asResolutionMap :: Declarations -> Map.Map Identifier Declaration
asResolutionMap Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = Map.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> Map.elems (_cdtFields cd)) (Map.elems dts)
   in asDeclsMap $ ds' <> dts'

asDeclsMap :: [Declaration] -> Map.Map Identifier Declaration
asDeclsMap ds = Map.fromList $ (\d -> (_declName d, d)) <$> ds

---

data Module = Module
  { _modName :: String,
    _hidingDecls :: [Declaration],
    _hidingCDs :: [ClassOrData]
  }
  deriving (Show, Generic, Eq)

$(makeLenses ''Module)

instance FromJSON Module

instance ToJSON Module

instance Semigroup Module where
  (<>) :: Module -> Module -> Module
  (<>) (Module n1 hd1 hcd1) (Module n2 hd2 hcd2) = Module (n1 <> n2) (hd1 <> hd2) (hcd1 <> hcd2)

instance Monoid Module where
  mempty :: Module
  mempty = Module "" [] []

---

data ExportsOrImports = ExportsOrImports
  { _eoiDecls :: [Declaration],
    _eoiModules :: [Module],
    _eoiCDs :: [ClassOrData]
  }
  deriving (Show, Generic, Eq)

$(makeLenses ''ExportsOrImports)

instance Semigroup ExportsOrImports where
  (<>) :: ExportsOrImports -> ExportsOrImports -> ExportsOrImports
  (<>) (ExportsOrImports es1 rs1 ed1) (ExportsOrImports es2 rs2 ed2) = ExportsOrImports (es1 <> es2) (rs1 <> rs2) (ed1 <> ed2)

instance Monoid ExportsOrImports where
  mempty :: ExportsOrImports
  mempty = ExportsOrImports [] [] []

asExports :: ExportsOrImports -> Exports
asExports ExportsOrImports {_eoiDecls = ds, _eoiModules = ms, _eoiCDs = cds} =
  Exports
    { exportedVars = ds,
      exportedModules = _modName <$> ms,
      exportedCDs = cds
    }

asImports :: ExportsOrImports -> Imports
asImports ExportsOrImports {_eoiDecls = ds, _eoiModules = ms, _eoiCDs = cds} =
  Imports
    { importedDecls = ds,
      importedModules = ms,
      importedCDs = cds
    }

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

---

data Imports = Imports
  { importedDecls :: [Declaration],
    importedModules :: [Module],
    importedCDs :: [ClassOrData]
  }
  deriving (Show, Generic, Eq)

instance FromJSON Imports

instance ToJSON Imports

instance Semigroup Imports where
  (<>) :: Imports -> Imports -> Imports
  (<>) (Imports is1 ims1 icd1) (Imports is2 ims2 icd2) = Imports (is1 <> is2) (ims1 <> ims2) (icd1 <> icd2)

instance Monoid Imports where
  mempty :: Imports
  mempty = Imports [] [] []

allImportedModules :: Imports -> [ModuleNameS]
allImportedModules (Imports ids ims icds) = deduplicate $ (_modName <$> ims) <> (_declModule <$> ids) <> (_declModule . _cdtName <$> icds)