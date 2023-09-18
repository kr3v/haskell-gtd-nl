{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module GTD.State where

import Control.Lens (makeLenses)
import qualified Data.Cache.LRU as LRU
import qualified Data.Map.Strict as Map
import GHC.Generics (Generic)
import GTD.Cabal.Get as Cabal (GetCache (..))
import GTD.Cabal.Types (ModuleNameS, PackageNameS)
import qualified GTD.Cabal.Types as Cabal
import GTD.Haskell.Declaration (Declarations)
import GTD.Haskell.Module (HsModuleP)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadState, MonadReader)
import GTD.Configuration (GTDConfiguration)

type LocalPackagesKey = (PackageNameS, Maybe String, Cabal.Version)

type LocalPackagesMap = Map.Map (PackageNameS, Maybe String) (Map.Map Cabal.Version Cabal.PackageWithUnresolvedDependencies)

data Context = Context
  { _ccFindAt :: Map.Map FilePath [Cabal.PackageWithUnresolvedDependencies],
    _ccFull :: Map.Map Cabal.PackageKey Cabal.PackageWithResolvedDependencies,
    _ccGet :: Cabal.GetCache,
    _cExports :: LRU.LRU Cabal.PackageKey (Map.Map ModuleNameS HsModuleP),
    _cResolution :: LRU.LRU (Cabal.PackageKey, FilePath) (Maybe (Map.Map ModuleNameS Declarations)),
    _cLocalPackages :: LocalPackagesMap
  }
  deriving (Show, Generic)

$(makeLenses ''Context)

emptyContext :: Context
emptyContext = Context mempty mempty (Cabal.GetCache mempty False) (LRU.newLRU Nothing) (LRU.newLRU $ Just 4) mempty

type MS m = (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m)
