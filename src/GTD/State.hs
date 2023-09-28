{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.State (module GTD.State, MS0) where

import Control.Lens (makeLenses)
import Control.Monad.RWS (MonadState)
import qualified Data.Cache.LRU as LRU
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal.Types as Cabal (GetCache (..), ModuleNameS, PackageNameS)
import qualified GTD.Cabal.Types as Cabal
import GTD.Configuration (MS0)
import GTD.Haskell.Declaration (Declarations)
import GTD.Haskell.Module (HsModuleP)
import qualified Data.HashMap.Strict as HMap

type LocalPackagesKey = (PackageNameS, Maybe String, Cabal.Version)

type LocalPackagesMap = Map.Map (PackageNameS, Maybe String) (Map.Map Cabal.Version Cabal.PackageWithUnresolvedDependencies)

data Context = Context
  { _ccFindAt :: Map.Map FilePath [Cabal.PackageWithUnresolvedDependencies],
    _ccFull :: Map.Map Cabal.PackageKey Cabal.PackageWithResolvedDependencies,
    _ccGet :: Cabal.GetCache,
    _cExports :: LRU.LRU Cabal.PackageKey (HMap.HashMap ModuleNameS HsModuleP),
    _cResolution :: LRU.LRU (Cabal.PackageKey, FilePath) (Maybe (HMap.HashMap ModuleNameS Declarations)),
    _cLocalPackages :: LocalPackagesMap
  }
  deriving (Show, Generic)

$(makeLenses ''Context)

emptyContext :: Context
emptyContext = Context mempty mempty (Cabal.GetCache mempty False) (LRU.newLRU Nothing) (LRU.newLRU $ Just 4) mempty


type MS m = (MS0 m, MonadState Context m)
