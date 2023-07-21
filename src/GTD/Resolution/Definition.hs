{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module GTD.Resolution.Definition where

import Control.Lens (use)
import Control.Monad (forM_)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadState (..))
import Control.Monad.Trans.Writer (execWriterT)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import GTD.Cabal (ModuleNameS)
import qualified GTD.Haskell.Declaration as Declarations
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, Imports (..), asDeclsMap, hasNonEmptyOrig)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..))
import qualified GTD.Haskell.Module as HsModule
import GTD.Resolution.State (Package (Package, _modules))
import qualified GTD.Resolution.State as Package
import GTD.Utils (logErrorNSS, mapFrom)
import Text.Printf (printf)

getExportedModule ::
  Package ->
  ModuleNameS ->
  Either String HsModuleP
getExportedModule Package {_modules = mods} modN = do
  case modN `Map.lookup` mods of
    Nothing -> Left $ printf "no package seem to export %s" (show modN)
    Just m -> Right m

getAllImports :: HsModule -> (MonadLoggerIO m, MonadState Package m) => m Declarations
getAllImports m' = do
  let logTag = "get all imports for " <> show (_name m')
  let Imports {importedDecls = iD, importedModules = iM, importedCDs = iCD} = HsModule._imports . _info $ m'
  ctx <- get
  let (errorsM, importsM) = partitionEithers $ getExportedModule ctx <$> iM
  forM_ errorsM (logErrorNSS logTag)
  return $
    Declarations
      { _decls = foldr (<>) (asDeclsMap iD) (Declarations._decls . HsModule._exports <$> importsM),
        _dataTypes = foldr (<>) (mapFrom (_declName . _cdtName) iCD) (Declarations._dataTypes . HsModule._exports <$> importsM)
      }

enrich0 ::
  (Show a) =>
  (a -> Map.Map ModuleNameS HsModuleP -> Maybe a) ->
  (a -> Bool) ->
  a ->
  (MonadLoggerIO m, MonadState Package m) => m a
enrich0 f p d = do
  mods <- use Package.modules
  return $ case f d mods of
    Nothing -> d
    Just x -> if p x then x else d

enrich :: HsModule -> (MonadLoggerIO m, MonadState Package m) => m Declarations
enrich m = do
  importsD <- getAllImports m
  importsE <- Map.elems <$> mapM (enrich0 (flip HsModule.resolve) hasNonEmptyOrig) (Declarations._decls importsD)
  importsCD <- Map.elems <$> mapM (enrich0 (flip HsModule.resolveCDT) (hasNonEmptyOrig . _cdtName)) (Declarations._dataTypes importsD)
  let locals = HsModule._locals . _info $ m
  return $ Declarations {_decls = asDeclsMap importsE <> Declarations._decls locals, _dataTypes = mapFrom (_declName . _cdtName) importsCD <> Declarations._dataTypes locals}

resolution :: Declarations -> Map.Map Identifier Declaration
resolution Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = Map.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> Map.elems (_cdtFields cd)) (Map.elems dts)
   in asDeclsMap $ ds' <> dts'
