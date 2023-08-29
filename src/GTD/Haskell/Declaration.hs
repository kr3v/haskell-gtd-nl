{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module GTD.Haskell.Declaration where

import Control.Lens (Each (..), makeLenses, over, (%=))
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal.Package (ModuleNameS)
import Language.Haskell.Exts (ModuleName (..), Name (..), SrcSpan (..), SrcSpanInfo (srcInfoSpan))
import Control.Monad.RWS (MonadState)
import Control.Monad.State (evalStateT, execState)

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

instance Binary SourceSpan

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

instance Binary Declaration

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
    _cdtFields :: Map.Map String Declaration,
    _eWildcard :: Bool
  }
  deriving (Show, Generic, Eq)

$(makeLenses ''ClassOrData)

instance FromJSON ClassOrData

instance ToJSON ClassOrData

instance Binary ClassOrData

---

data Declarations = Declarations
  { _decls :: Map.Map String Declaration,
    _dataTypes :: Map.Map String ClassOrData
  }
  deriving (Show, Generic, Eq)

$(makeLenses ''Declarations)

instance FromJSON Declarations

instance ToJSON Declarations

instance Binary Declarations

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

declarationsT :: (Declaration -> Declaration) -> (Declarations -> Declarations)
declarationsT d = execState (declarationsTS d)

declarationsTS :: (Declaration -> Declaration) -> (MonadState Declarations m) => m ()
declarationsTS d = do
  (decls . each) %= d
  (dataTypes . each . cdtFields . each) %= d
  (dataTypes . each . cdtName) %= d

---

data ModuleImportType = All | Exactly | EverythingBut deriving (Show, Generic, Eq)

instance ToJSON ModuleImportType

instance FromJSON ModuleImportType

data Module = Module
  { _mName :: ModuleNameS,
    _mQualifier :: String,
    _mAllowNoQualifier :: Bool,
    _mType :: ModuleImportType,
    _mDecls :: [Declaration],
    _mCDs :: [ClassOrData]
  }
  deriving (Show, Generic, Eq)

$(makeLenses ''Module)

instance FromJSON Module

instance ToJSON Module

instance Semigroup Module where
  (<>) :: Module -> Module -> Module
  (<>) (Module mn1 q1 aq1 t1 ds1 cds1) (Module mn2 q2 aq2 t2 ds2 cds2) =
    Module mn1 q1 (aq1 && aq2) t1 (ds1 <> ds2) (cds1 <> cds2)

instance Monoid Module where
  mempty :: Module
  mempty = Module "" "" True All [] []

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

---

type Exports = Map.Map ModuleNameS Module

type Imports = [Module]

allImportedModules :: Imports -> [ModuleNameS]
allImportedModules = fmap _mName
