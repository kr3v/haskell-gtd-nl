{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GTD.Resolution.Module
  ( modules,
    figureOutExports'old, -- used from the debugging module and tests
    orderedByDependencies, -- used from the debugging module
    ModuleState (..), -- used from tests
    figureOutExports, -- used from tests
  )
where

import Control.Lens (makeLenses, (%~))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (..))
import Control.Monad.State.Lazy (MonadState (..), modify)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON (..), KeyValue (..), ToJSON (..), Value, object, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal.Types (ModuleNameS)
import qualified GTD.Cabal.Types as Cabal (Package (..), PackageWithResolvedDependencies, key, _exports)
import GTD.Configuration (GTDConfiguration, MS0)
import GTD.Haskell.Declaration (SourceSpan (..), SourceSpanFileName)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..))
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.Module.Multi (collectUsages, figureOutExports0, resolution)
import GTD.Resolution.Module.Single (module'Dependencies, moduleR)
import qualified GTD.Resolution.Types as Package (Package (..))
import GTD.Resolution.Utils (ParallelizedState (ParallelizedState), parallelized, scheme)
import GTD.Utils (restrictKeys)

data ModuleState = ModuleState
  { _mods :: HMap.HashMap ModuleNameS HsModuleP,
    _usages :: HMap.HashMap SourceSpanFileName (HMap.HashMap SourceSpan [SourceSpan])
  }
  deriving (Generic)

$(makeLenses ''ModuleState)

instance ToJSON ModuleState where
  toJSON :: ModuleState -> Value
  toJSON (ModuleState m u) =
    object
      [ fromString "_mods" .= m,
        fromString "_usages" .= HMap.toList (HMap.toList <$> u)
      ]

instance FromJSON ModuleState where
  parseJSON :: Value -> Parser ModuleState
  parseJSON = withObject "ModuleState" $ \v -> do
    m <- v .: fromString "_mods"
    uL <- v .: fromString "_usages"
    let u = HMap.fromList <$> HMap.fromList uL
    return $ ModuleState m u

instance Binary ModuleState

figureOutExports'old ::
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState ModuleState m) => m HsModuleP
figureOutExports'old m = do
  st <- get
  (r, s) <- figureOutExports st m
  modify s
  return r

figureOutExports ::
  ModuleState ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (HsModuleP, ModuleState -> ModuleState)
figureOutExports st m = do
  liM <- resolution (_mods st) m
  (r, mM) <- figureOutExports0 liM m
  let uM = collectUsages m liM
  return $ (r,) $ (mods %~ mM) . (usages %~ uM)

---

-- for a given Cabal package, it returns a list of modules in the order they should be processed
orderedByDependencies :: Cabal.PackageWithResolvedDependencies -> (MS0 m) => m [HsModule]
orderedByDependencies c =
  scheme (moduleR c) HsModule._name id (return . module'Dependencies) (Set.toList . Cabal._exports . Cabal._modules $ c)

-- for a given Cabal package and list of its modules in the 'right' order, concurrently parses all the modules
modules ::
  Package.Package ->
  (MS0 m) => m Package.Package
modules pkg = do
  let c = Package._cabalPackage pkg
  modsO <- orderedByDependencies c
  r <-
    parallelized
      (ParallelizedState modsO Set.empty Map.empty (ModuleState (Package._modules pkg) HMap.empty) False)
      ("modules", Cabal.key c)
      (\a b -> snd <$> figureOutExports a b)
      (show . HMap.keys . _mods)
      HsModule._name
      (return . module'Dependencies)
  let ms = _mods r
  return
    pkg
      { Package._exports = restrictKeys ms (Cabal._exports . Cabal._modules $ c),
        Package._modules = ms,
        Package._usages = _usages r
      }
