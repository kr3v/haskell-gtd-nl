{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Module where

import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (StateT (runStateT))
import Control.Monad.Trans.Writer (execWriterT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import qualified GTD.Cabal.Types as Cabal
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (ClassOrData (_cdtName), Declaration (..), Declarations (..), Exports, Imports, SourceSpan (..), declarationsT)
import qualified GTD.Haskell.Declaration as Declarations
import qualified GTD.Haskell.Lines as Lines
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import GTD.Utils (logDebugNSS, storeIOExceptionToMonadError)
import Language.Haskell.Exts (Module (Module), SrcSpan (..), SrcSpanInfo (..))

newtype HsModuleParams = HsModuleParams
  { _isImplicitExportAll :: Bool
  }
  deriving (Show, Eq, Generic)

emptyParams :: HsModuleParams
emptyParams = HsModuleParams {_isImplicitExportAll = False}

instance FromJSON HsModuleParams

instance ToJSON HsModuleParams

data HsModuleData = HsModuleData
  { _exports0 :: Exports,
    _imports :: Imports,
    _locals :: Declarations
  }
  deriving (Show, Eq, Generic)

instance FromJSON HsModuleData

instance ToJSON HsModuleData

emptyData :: HsModuleData
emptyData = HsModuleData {_exports0 = Map.empty, _imports = [], _locals = Declarations {_decls = Map.empty, _dataTypes = Map.empty}}

data HsModule = HsModule
  { _package :: Cabal.PackageNameS,
    _pkgK :: Cabal.PackageKey,
    _name :: Cabal.ModuleNameS,
    _path :: FilePath,
    _info :: HsModuleData,
    _params :: HsModuleParams,
    _lines :: Lines.Lines
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''HsModule)

instance FromJSON HsModule

instance ToJSON HsModule

emptySrcSpan :: SrcSpan
emptySrcSpan = SrcSpan {srcSpanFilename = "", srcSpanStartLine = 0, srcSpanStartColumn = 0, srcSpanEndLine = 0, srcSpanEndColumn = 0}

emptySrcSpanInfo :: SrcSpanInfo
emptySrcSpanInfo = SrcSpanInfo {srcInfoSpan = emptySrcSpan, srcInfoPoints = []}

emptyHaskellModule :: Module SrcSpanInfo
emptyHaskellModule = Module emptySrcSpanInfo Nothing [] [] []

emptyHsModule :: HsModule
emptyHsModule =
  HsModule
    { _package = "",
      _name = "",
      _pkgK = Cabal.emptyPackageKey,
      _path = "",
      _info = emptyData,
      _params = emptyParams,
      _lines = mempty
    }

parseModule :: HsModule -> (MonadLoggerIO m, MonadError String m) => m HsModule
parseModule cm@HsModule {_path = srcP} = do
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
  (iiea, es) <- runStateT (GHC.exports a) Map.empty
  is <- execWriterT $ GHC.imports a
  localsO <- execWriterT $ GHC.identifiers a
  locals <- flip declarationsT localsO $ \d -> do
    case Lines.resolve lines (sourceSpanStartLine . _declSrcOrig $ d) of
      Just Lines.Line {path = p, num = n} -> d {_declSrcOrig = (_declSrcOrig d) {sourceSpanFileName = p, sourceSpanStartLine = n, sourceSpanEndLine = n}}
      Nothing -> d
  return $
    cm
      { _info = HsModuleData {_exports0 = es, _imports = is, _locals = locals},
        _params = HsModuleParams {_isImplicitExportAll = iiea},
        _name = GHC.name a,
        _lines = lines
      }

---

newtype HsModuleP = HsModuleP
  { _exports :: Declarations
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''HsModuleP)

instance FromJSON HsModuleP

instance ToJSON HsModuleP

instance Binary HsModuleP

---

resolve :: Map.Map Cabal.ModuleNameS HsModuleP -> Declaration -> Maybe Declaration
resolve moduleDecls orig = do
  m <- _declModule orig `Map.lookup` moduleDecls
  _declName orig `Map.lookup` (Declarations._decls . _exports) m

resolveCDT :: Map.Map Cabal.ModuleNameS HsModuleP -> ClassOrData -> Maybe ClassOrData
resolveCDT moduleDecls orig = do
  m <- (_declModule . _cdtName) orig `Map.lookup` moduleDecls
  (_declName . _cdtName) orig `Map.lookup` (Declarations._dataTypes . _exports) m
