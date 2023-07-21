{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}

module GTD.Haskell.Declaration where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import Language.Haskell.Exts (ModuleName (..), Name (..), SrcSpan (..), SrcSpanInfo (srcInfoSpan))
import qualified Data.Map.Strict as Map
import GTD.Utils (deduplicate)

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
    importedModules :: [ModuleNameS],
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
allImportedModules (Imports ids ims icds) = deduplicate $ ims <> (_declModule <$> ids) <> (_declModule . _cdtName <$> icds)