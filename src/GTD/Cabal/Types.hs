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
{-# LANGUAGE RecordWildCards #-}

module GTD.Cabal.Types where

import Control.Applicative (Applicative (..))
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import Distribution.Compat.Prelude (Binary, Generic, fromMaybe)
import Distribution.Pretty (prettyShow)
import Distribution.Types.VersionRange (VersionRange)
import qualified Distribution.Version as Cabal
import System.Directory (doesFileExist)
import Control.Monad (forM)
import System.FilePath ((</>))
import Data.List (find)
import Distribution.ModuleName (toFilePath, fromString)

type PackageNameS = String

type ModuleNameS = String

data DesignationType = Library | Executable | TestSuite | Benchmark
  deriving (Eq, Ord, Read, Show, Generic)

data Designation = Designation {_desName :: Maybe String, _desType :: DesignationType}
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Designation

instance FromJSON DesignationType

instance ToJSON Designation

instance ToJSON DesignationType

instance Binary DesignationType

instance Binary Designation

dKeyType :: DesignationType -> String
dKeyType Library = "lib"
dKeyType Executable = "exe"
dKeyType TestSuite = "test"
dKeyType Benchmark = "bench"

dKey :: Designation -> String
dKey Designation{..} = dKeyType _desType ++ ":" ++ fromMaybe "" _desName

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
    _projectRoot :: FilePath,
    _path :: FilePath,
    _modules :: PackageModules,
    _dependencies :: [a]
  }
  deriving (Eq, Show, Generic)

$(makeLenses ''Package)

transformPathsR :: (FilePath -> FilePath) -> PackageWithResolvedDependencies -> PackageWithResolvedDependencies
transformPathsR f p = transformPaths f p {_dependencies = transformPaths f <$> _dependencies p}

transformPaths :: (FilePath -> FilePath) -> Package a -> Package a
transformPaths f p = p {_root = f (_root p), _path = f (_path p)}

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

isMainLib :: Package a -> Bool
isMainLib = liftA2 (&&) (isNothing . _desName) ((== Library) . _desType) . _designation

resolve :: Package a -> ModuleNameS -> IO (Maybe FilePath)
resolve p m = do
  let mp = toFilePath . fromString $ m
  let ds = fmap (\d -> _root p </> d </> (mp ++ ".hs")) $ _srcDirs . _modules $ p
  xs <- forM ds $ \d -> (d,) <$> doesFileExist d
  return $ fst <$> find snd xs