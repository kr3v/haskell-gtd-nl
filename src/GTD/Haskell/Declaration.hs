{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GTD.Haskell.Declaration where

import Control.Applicative (Applicative (liftA2))
import Control.DeepSeq (NFData)
import Control.Lens (Each (..), makeLenses, (%=))
import Control.Monad.RWS (MonadState, MonadTrans (lift))
import Control.Monad.State (execStateT)
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey)
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HMap
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal.Types (ModuleNameS)
import GTD.Utils (modifyEachM, overM)
import GTD.Utils.Orphans ()

instance (Hashable k, Eq k, Binary k, Binary v) => Binary (HMap.HashMap k v) where
  get :: (Hashable k, Eq k, Binary k, Binary v) => Binary.Get (HMap.HashMap k v)
  get = fmap HMap.fromList Binary.get
  put :: (Hashable k, Eq k, Binary k, Binary v) => HMap.HashMap k v -> Binary.Put
  put = Binary.put . HMap.toList

type SourceSpanFileName = ByteString

data SourceSpan = SourceSpan
  { _fileName :: SourceSpanFileName,
    _lineBegin :: Int,
    _lineEnd :: Int,
    _colBegin :: Int,
    _colEnd :: Int
  }
  deriving (NFData, Show, Generic, Eq, Ord, Hashable)

emptySourceSpan :: SourceSpan
emptySourceSpan = SourceSpan mempty 0 0 0 0

instance ToJSON SourceSpan

instance FromJSON SourceSpan

instance Binary SourceSpan

---

data Declaration = Declaration
  { _declSrcOrig :: SourceSpan,
    _declSrcOthers :: [SourceSpan],
    _declModule :: ModuleNameS,
    _declName :: BSC8.ByteString
  }
  deriving (NFData, Show, Eq, Generic, Ord)

$(makeLenses ''Declaration)

instance FromJSON Declaration

instance ToJSON Declaration

instance Binary Declaration

instance Semigroup Declaration where
  (<>) :: Declaration -> Declaration -> Declaration
  (<>) (Declaration s1 o1 m1 n1) (Declaration s2 o2 m2 n2) = Declaration s1 (o1 <> [s2] <> o2) m1 n1

instance Monoid Declaration where
  mempty :: Declaration
  mempty = Declaration emptySourceSpan [] mempty mempty

hasNonEmptyOrig :: Declaration -> Bool
hasNonEmptyOrig = (/= emptySourceSpan) . _declSrcOrig

---

type Identifier = BSC8.ByteString

---

data ClassOrData = ClassOrData
  { _cdtName :: Declaration,
    _cdtFields :: HMap.HashMap Identifier Declaration,
    _eWildcard :: Bool
  }
  deriving (NFData, Show, Generic, Eq)

$(makeLenses ''ClassOrData)

instance FromJSON ClassOrData

instance ToJSON ClassOrData

instance Binary ClassOrData

---

data Declarations = Declarations
  { _decls :: HMap.HashMap Identifier Declaration,
    _dataTypes :: HMap.HashMap Identifier ClassOrData
  }
  deriving (NFData, Show, Generic, Eq)

$(makeLenses ''Declarations)

instance FromJSON Declarations

instance ToJSON Declarations

instance Binary Declarations

instance Semigroup Declarations where
  (<>) :: Declarations -> Declarations -> Declarations
  (<>) (Declarations d1 dt1) (Declarations d2 dt2) = Declarations (HMap.unionWith (<>) d1 d2) (dt1 <> dt2)

instance Monoid Declarations where
  mempty :: Declarations
  mempty = Declarations mempty mempty

declsF :: Declarations -> [Declaration]
declsF Declarations {_decls = ds, _dataTypes = dts} = do
  let ds' = HMap.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> HMap.elems (_cdtFields cd)) (HMap.elems dts)
   in ds' <> dts'

srcSpans :: Declarations -> [SourceSpan]
srcSpans d = concatMap (liftA2 (<>) ((: []) . _declSrcOrig) _declSrcOthers) (declsF d)

asDeclsMap :: [Declaration] -> Map.Map Identifier Declaration
asDeclsMap ds = Map.fromListWith (<>) $ (\d -> (_declName d, d)) <$> ds

asDeclsHMap :: [Declaration] -> HMap.HashMap Identifier Declaration
asDeclsHMap ds = HMap.fromListWith (<>) $ (\d -> (_declName d, d)) <$> ds

declarationsT :: (Monad m) => (Declaration -> Declaration) -> Declarations -> m Declarations
declarationsT d = execStateT (declarationsTS d)

declarationsTS :: (Declaration -> Declaration) -> (MonadState Declarations m) => m ()
declarationsTS d = do
  (decls . each) %= d
  (dataTypes . each . cdtFields . each) %= d
  (dataTypes . each . cdtName) %= d

declarationsMT :: (Monad m) => (Declaration -> m Declaration) -> Declarations -> m Declarations
declarationsMT d = execStateT $ do
  let dS = lift . d
  modifyEachM decls dS
  modifyEachM dataTypes $ \(x :: ClassOrData) -> flip execStateT x $ do
    modifyEachM cdtFields $ lift . lift . d
    overM cdtName $ lift . lift . d

data ModuleImportType = All | Exactly | EverythingBut deriving (NFData, Show, Generic, Eq)

instance ToJSON ModuleImportType

instance FromJSON ModuleImportType

instance Binary ModuleImportType

data Module = Module
  { _mName :: ModuleNameS,
    _mQualifier :: BSC8.ByteString,
    _mAllowNoQualifier :: Bool,
    _mType :: ModuleImportType,
    _mDecls :: [Declaration],
    _mCDs :: [ClassOrData]
  }
  deriving (NFData, Show, Generic, Eq)

$(makeLenses ''Module)

instance FromJSON Module

instance ToJSON Module

instance Binary Module

instance Semigroup Module where
  (<>) :: Module -> Module -> Module
  (<>) (Module mn1 q1 aq1 t1 ds1 cds1) (Module mn2 q2 aq2 t2 ds2 cds2) =
    Module mn1 q1 (aq1 && aq2) t1 (ds1 <> ds2) (cds1 <> cds2)

instance Monoid Module where
  mempty :: Module
  mempty = Module mempty mempty True All [] []

---

data ExportsOrImports = ExportsOrImports
  { _eoiDecls :: [Declaration],
    _eoiModules :: [Module],
    _eoiCDs :: [ClassOrData]
  }
  deriving (NFData, Show, Generic, Eq)

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

---

data UsageType = Regular | Import | Export | UDeclaration deriving (Ord, Hashable, Enum, Eq, Show, Generic, NFData)

instance FromJSON UsageType

instance ToJSON UsageType

instance ToJSONKey UsageType

instance FromJSONKey UsageType

instance Binary UsageType

data IdentifierWithUsageLocation = IdentifierUsage
  { _iuModule :: ByteString,
    _iuName :: ByteString,
    _iuType :: UsageType,
    _iuSourceSpan :: SourceSpan
  }
  deriving (NFData, Show, Eq, Generic)

$(makeLenses ''IdentifierWithUsageLocation)

instance FromJSON IdentifierWithUsageLocation

instance ToJSON IdentifierWithUsageLocation

instance Binary IdentifierWithUsageLocation