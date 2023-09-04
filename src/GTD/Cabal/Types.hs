{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module GTD.Cabal.Types where

import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as Set
import Distribution.Compat.Prelude (Binary, Generic, fromMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.Types.VersionRange (VersionRange)
import qualified Distribution.Version as Cabal

type PackageNameS = String

type ModuleNameS = String

data DesignationType = Library | Executable | TestSuite | Benchmark
  deriving (Eq, Ord, Show, Generic)

data Designation = Designation {_desName :: Maybe String, _desType :: DesignationType}
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Designation

instance FromJSON DesignationType

instance ToJSON Designation

instance ToJSON DesignationType

instance Binary DesignationType

instance Binary Designation

dKey :: Designation -> String
dKey d = case _desType d of
  Library -> "lib:" ++ fromMaybe "" (_desName d)
  Executable -> "exe:" ++ fromMaybe "" (_desName d)
  TestSuite -> "test:" ++ fromMaybe "" (_desName d)
  Benchmark -> "bench:" ++ fromMaybe "" (_desName d)

data Dependency = Dependency
  { _dName :: PackageNameS,
    _dVersion :: VersionRange,
    _dSubname :: Maybe String
  }
  deriving (Show, Eq, Ord, Generic)

$(makeLenses ''Dependency)

instance Binary Dependency

data PackageModules = PackageModules
  { _srcDirs :: [FilePath],
    _allKnownModules :: Set.Set ModuleNameS,
    _exports :: Set.Set ModuleNameS,
    _reExports :: Set.Set ModuleNameS
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''PackageModules)

emptyPackageModules :: PackageModules
emptyPackageModules = PackageModules [] Set.empty Set.empty Set.empty

instance Binary PackageModules

type PackageWithResolvedDependencies = Package (Package Dependency)

type PackageWithUnresolvedDependencies = Package Dependency

type Version = Cabal.Version

data Package a = Package
  { _designation :: Designation,
    _name :: PackageNameS,
    _version :: Version,
    _root :: FilePath,
    _path :: FilePath,
    _modules :: PackageModules,
    _dependencies :: [a]
  }
  deriving (Eq, Show, Generic)

instance Binary (Package (Package Dependency))

instance Binary (Package Dependency)

data PackageKey = PackageKey
  { _pkName :: PackageNameS,
    _pkVersion :: String,
    _pkDesignation :: Designation
  }
  deriving (Show, Eq, Ord, Generic)

instance Binary PackageKey

instance FromJSON PackageKey

instance ToJSON PackageKey

emptyPackageKey :: PackageKey
emptyPackageKey = PackageKey "" "" (Designation Nothing Library)

key :: Package a -> PackageKey
key p = PackageKey {_pkName = _name p, _pkVersion = prettyShow . _version $ p, _pkDesignation = _designation p}

pKey :: PackageKey -> String
pKey k = _pkName k ++ "-" ++ _pkVersion k ++ "-" ++ dKey (_pkDesignation k)
