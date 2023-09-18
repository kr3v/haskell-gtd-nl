{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Resolution.Types where

import Control.Lens (makeLenses)
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal.Types (ModuleNameS)
import qualified GTD.Cabal.Types as Cabal
import GTD.Haskell.Module (HsModuleP)

data Package = Package
  { _cabalPackage :: Cabal.PackageWithResolvedDependencies,
    _modules :: Map.Map ModuleNameS HsModuleP,
    _exports :: Map.Map ModuleNameS HsModuleP
  }
  deriving (Show, Generic)

$(makeLenses ''Package)