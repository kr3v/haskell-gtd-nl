{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Resolution.Types where

import Control.Lens (makeLenses)
import qualified Data.HashMap.Strict as HMap
import GHC.Generics (Generic)
import GTD.Cabal.Types (ModuleNameS)
import qualified GTD.Cabal.Types as Cabal
import GTD.Haskell.Declaration (SourceSpan, SourceSpanFileName)
import GTD.Haskell.Module (HsModuleP)

type UsagesMap = HMap.HashMap SourceSpanFileName UsagesInFileMap

type UsagesInFileMap = HMap.HashMap SourceSpan [SourceSpan]

data Package = Package
  { _cabalPackage :: Cabal.PackageWithResolvedDependencies,
    _modules :: HMap.HashMap ModuleNameS HsModuleP,
    _exports :: HMap.HashMap ModuleNameS HsModuleP,
    _usages :: HMap.HashMap SourceSpanFileName (HMap.HashMap SourceSpan [SourceSpan])
  }
  deriving (Show, Generic)

$(makeLenses ''Package)