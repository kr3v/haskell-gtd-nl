{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module GTD.Haskell.Module where

import Control.Exception (IOException, try)
import Control.Lens (makeLenses)
import Control.Monad.Cont (MonadIO)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.State (MonadIO (..))
import Control.Monad.Trans.Writer
import Data.Aeson (FromJSON, ToJSON)
import Data.Either.Combinators (mapLeft)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS, PackageNameS)
import GTD.Haskell.AST (ClassOrData (_cdtName), Declarations (..), Exports (..), Imports (Imports, importedCDs, importedDecls, importedModules), haskellGetExports, haskellGetIdentifiers, haskellGetImports, haskellParse)
import qualified GTD.Haskell.AST as Declarations
import GTD.Haskell.Cpphs (haskellApplyCppHs)
import GTD.Haskell.Declaration (Declaration (_declModule, _declName))
import GTD.Utils (logDebugNSS)
import Language.Haskell.Exts (Module (Module), SrcSpan (..), SrcSpanInfo (..))

data HsModuleData = HsModuleData
  { _isImplicitExportAll :: Bool,
    _exports0 :: Exports,
    _imports :: Imports,
    _locals :: Declarations
  }
  deriving (Show, Eq, Generic)

emptyData :: HsModuleData
emptyData = HsModuleData {_isImplicitExportAll = False, _exports0 = Exports {exportedModules = [], exportedVars = [], exportedCDs = []}, _imports = Imports {importedModules = [], importedDecls = [], importedCDs = []}, _locals = Declarations {_decls = Map.empty, _dataTypes = Map.empty}}

data HsModule = HsModule
  { _package :: PackageNameS,
    _name :: ModuleNameS,
    _path :: FilePath,
    _info :: HsModuleData
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''HsModule)

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
      _path = "",
      _info = emptyData
    }

parseModule :: HsModule -> (MonadLoggerIO m, MonadError String m) => m HsModule
parseModule cm = do
  let srcP = _path cm
  let logTag = "parsing module " ++ srcP
  logDebugNSS logTag ""

  src <- liftEither . mapLeft show =<< (liftIO (try $ readFile srcP) :: (MonadIO m) => m (Either IOException String))
  srcPostCpp <- haskellApplyCppHs srcP src
  a <- liftEither $ haskellParse srcP srcPostCpp

  (iiea, es) <- runWriterT $ haskellGetExports a
  is <- execWriterT $ haskellGetImports a
  locals <- execWriterT $ haskellGetIdentifiers a

  return $ cm {_info = HsModuleData {_isImplicitExportAll = iiea, _exports0 = es, _imports = is, _locals = locals}}

---

newtype HsModuleP = HsModuleP
  { _exports :: Declarations
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''HsModuleP)

instance FromJSON HsModuleP

instance ToJSON HsModuleP

---

resolve :: Map.Map ModuleNameS HsModuleP -> Declaration -> Maybe Declaration
resolve moduleDecls orig = do
  m <- _declModule orig `Map.lookup` moduleDecls
  _declName orig `Map.lookup` (Declarations._decls . _exports) m

resolveCDT :: Map.Map ModuleNameS HsModuleP -> ClassOrData -> Maybe ClassOrData
resolveCDT moduleDecls orig = do
  m <- (_declModule . _cdtName) orig `Map.lookup` moduleDecls
  (_declName . _cdtName) orig `Map.lookup` (Declarations._dataTypes . _exports) m
