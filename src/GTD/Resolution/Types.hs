{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Resolution.Types where

import Control.Lens (makeLenses)
import qualified Data.HashMap.Strict as HMap
import GHC.Generics (Generic)
import GTD.Cabal.Types (ModuleNameS)
import qualified GTD.Cabal.Types as Cabal
import GTD.Haskell.Module (HsModuleP)
import GTD.Resolution.Module.Types (UsagesMap)


data Package = Package
  { _cabalPackage :: Cabal.PackageWithResolvedDependencies,
    _modules :: HMap.HashMap ModuleNameS HsModuleP,
    _exports :: HMap.HashMap ModuleNameS HsModuleP,
    _usages :: UsagesMap
  }
  deriving (Show, Generic)

$(makeLenses ''Package)