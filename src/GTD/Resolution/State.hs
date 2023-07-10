{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Resolution.State where

import Control.Lens (makeLenses)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Haskell.Module (HsModuleP)

data Package = Package
  { _cabalPackage :: Cabal.PackageFull,
    _modules :: Map.Map ModuleNameS HsModuleP,
    _exports :: Map.Map ModuleNameS HsModuleP
  }
  deriving (Show, Generic)

$(makeLenses ''Package)

---

data Context = Context
  { _ccFindAt :: Map.Map FilePath Cabal.PackageFull,
    _ccFull :: Map.Map Cabal.PackageWithVersion Cabal.PackageFull,
    _ccGet :: Cabal.GetCache,
    _versions :: Map.Map Cabal.PackageWithVersionP Cabal.PackageWithVersion,
    _cExports :: Map.Map Cabal.PackageWithVersion (Map.Map ModuleNameS HsModuleP)
  }
  deriving (Show, Generic)

$(makeLenses ''Context)

emptyContext :: Context
emptyContext = Context Map.empty Map.empty (Cabal.GetCache Map.empty False) Map.empty Map.empty
