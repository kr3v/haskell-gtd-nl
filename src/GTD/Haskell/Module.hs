{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Module where

import Control.DeepSeq (NFData, deepseq)
import Control.Lens (Each (..), makeLenses, (%~))
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Control.Monad.Trans.Writer (execWriterT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (Args (..), GTDConfiguration (_args), MS0, Powers (..), shouldCollectDataForGoToReferences)
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (ClassOrData (_cdtName), Declaration (..), Declarations (..), Exports, IdentifierWithUsageLocation, Imports, SourceSpan (..), declSrcOrig, declSrcOthers, declarationsT, iuSourceSpan)
import qualified GTD.Haskell.Declaration as Declaration
import qualified GTD.Haskell.Lines as Lines
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Utils (logDebugNSS, storeIOExceptionToMonadError)

newtype HsModuleParams = HsModuleParams
  { _isImplicitExportAll :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON HsModuleParams

instance ToJSON HsModuleParams

instance Binary HsModuleParams

emptyParams :: HsModuleParams
emptyParams = HsModuleParams {_isImplicitExportAll = False}

data HsModuleData = HsModuleData
  { _exports0 :: Exports,
    _imports :: Imports,
    _locals :: Declarations,
    _identifierUsages :: [IdentifierWithUsageLocation]
  }
  deriving (Show, Eq, Generic)

instance FromJSON HsModuleData

instance ToJSON HsModuleData

instance Binary HsModuleData

emptyData :: HsModuleData
emptyData =
  HsModuleData
    { _exports0 = Map.empty,
      _imports = [],
      _locals = Declarations {_decls = Map.empty, _dataTypes = Map.empty},
      _identifierUsages = []
    }

data HsModuleMetadata = HsModuleMetadata
  { _mPackage :: Cabal.PackageNameS,
    _mPkgK :: Cabal.PackageKey,
    _mName :: Cabal.ModuleNameS,
    _mPath :: FilePath
  }
  deriving (NFData, Show, Eq, Generic)

instance FromJSON HsModuleMetadata

instance ToJSON HsModuleMetadata

instance Binary HsModuleMetadata

emptyMetadata :: HsModuleMetadata
emptyMetadata = HsModuleMetadata {_mPackage = "", _mPkgK = Cabal.emptyPackageKey, _mName = "", _mPath = ""}

metadataPrettyShow :: HsModuleMetadata -> String
metadataPrettyShow m = show (_mPkgK m) ++ ":" ++ show (_mName m)

data HsModule = HsModule
  { _metadata :: HsModuleMetadata,
    _info :: HsModuleData,
    _params :: HsModuleParams,
    _lines :: Lines.Lines
  }
  deriving (Show, Eq, Generic)

instance FromJSON HsModule

instance ToJSON HsModule

_name :: HsModule -> Cabal.ModuleNameS
_name = _mName . _metadata

_path :: HsModule -> FilePath
_path = _mPath . _metadata

_package :: HsModule -> Cabal.PackageNameS
_package = _mPackage . _metadata

_pkgK :: HsModule -> Cabal.PackageKey
_pkgK = _mPkgK . _metadata

$(makeLenses ''HsModule)

emptyHsModule :: HsModule
emptyHsModule =
  HsModule
    { _metadata = emptyMetadata,
      _info = emptyData,
      _params = emptyParams,
      _lines = mempty
    }

parseModule :: HsModuleMetadata -> (MS0 m, MonadError String m) => m HsModule
parseModule cm@HsModuleMetadata {_mPath = srcP} = do
  let logTag = "parsing module " ++ srcP
  logDebugNSS logTag ""

  src <- storeIOExceptionToMonadError $ readFile srcP
  srcPostCpp <- haskellApplyCppHs srcP src
  let lines = Lines.buildMap srcPostCpp
  let srcPostLines = Lines.dropDirectives srcPostCpp

  aE <- GHC.parse srcP srcPostLines
  a <- case aE of
    Left err -> throwError err
    Right a -> return a
  let es = GHC.exports a
  let iiea = GHC.isImplicitExportAll a
  is <- execWriterT $ GHC.imports a
  localsO <- execWriterT $ GHC.identifiers a
  locals <- declarationsT ((declSrcOrig %~ Lines.translate lines) . ((declSrcOthers . each) %~ Lines.translate lines)) localsO

  e <- asks $ shouldCollectDataForGoToReferences . _powers . _args
  let ids =
        if not e
          then []
          else
            GHC.identifierUsages'declarations locals $
              (iuSourceSpan %~ Lines.translate lines) <$> GHC.identifierUsages'raw a

  _ <- liftIO $ deepseq ids $ deepseq locals $ deepseq es $ deepseq is $ deepseq iiea $ return ()

  return $
    HsModule
      { _info = HsModuleData {_exports0 = es, _imports = is, _locals = locals, _identifierUsages = ids},
        _params = HsModuleParams {_isImplicitExportAll = iiea},
        _metadata = cm {_mName = GHC.name a},
        _lines = lines
      }

---

data HsModuleP = HsModuleP
  { _exports :: Declarations,
    _ometadata :: HsModuleMetadata
  }
  deriving (NFData, Show, Eq, Generic)

$(makeLenses ''HsModuleP)

instance FromJSON HsModuleP

instance ToJSON HsModuleP

instance Binary HsModuleP

