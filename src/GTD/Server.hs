{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Server where

import Control.Lens (At (at), makeLenses, use, view, (%=), (.=))
import Control.Monad (forM_, guard)
import Control.Monad.Except (ExceptT, MonadError (throwError), liftEither, runExceptT)
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadIO (..), MonadReader, MonadState (..), modify)
import Control.Monad.State (execState)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Writer (WriterT (runWriterT), execWriterT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Distribution.Compat.Directory (listDirectory)
import GHC.Generics (Generic)
import GTD.Cabal (CabalPackage (..), ModuleNameS, PackageNameS, cabalDeps, cabalPackageName, cabalPackagePath, cabalRead)
import GTD.Configuration (GTDConfiguration)
import GTD.Haskell.AST (ClassOrData (..), Declarations (..), Imports (..), haskellGetIdentifiers, haskellGetImports)
import qualified GTD.Haskell.AST as Declarations
import GTD.Haskell.Declaration (Declaration (..), Identifier, SourceSpan, hasNonEmptyOrig)
import GTD.Haskell.Enrich (enrichTryPackage, enrichTryPackageCDT)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Haskell.Package (Package (..), ccpmodules, dependencies)
import qualified GTD.Haskell.Package as Package
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (deduplicateBy, logDebugNSS, logErrorNSS, mapFrom, ultraZoom)
import System.FilePath (takeExtension, (</>))
import Text.Printf (printf)

---

-- FIXME: use `mtime`
data CabalCacheEntry = CabalCacheEntry
  { _mtime :: Integer,
    _deps :: [CabalPackage]
  }
  deriving (Show, Generic)

$(makeLenses ''CabalCacheEntry)

cabalDependencies' ::
  FilePath ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadError String m) => m [CabalPackage]
cabalDependencies' wd = do
  cabalFiles <- liftIO $ filter (\x -> takeExtension x == ".cabal") <$> listDirectory wd
  cabalFile <- case length cabalFiles of
    0 -> throwError "No cabal file found"
    1 -> return $ wd </> head cabalFiles
    _ -> throwError "Multiple cabal files found"
  logDebugNSS "definition" $ "Found cabal file: " ++ cabalFile
  pkg <- cabalRead cabalFile
  deduplicateBy (view cabalPackagePath) <$> cabalDeps pkg

type CabalCache = Map.Map FilePath [CabalPackage]

cabalDependencies ::
  FilePath ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState CabalCache m, MonadError String m) => m [CabalPackage]
cabalDependencies wd = do
  e <- use $ at wd
  case e of
    Just d -> return d
    Nothing -> do
      d <- cabalDependencies' wd
      modify $ Map.insert wd d
      return d

---

data ServerState = ServerState
  { _context :: Package,
    _cabalPackages :: CabalCache,
    _reqId :: Int
  }

$(makeLenses ''ServerState)

emptyServerState :: ServerState
emptyServerState = ServerState {_context = Package {_ccpmodules = Map.empty, _dependencies = [], _cabalCache = Map.empty}, _cabalPackages = Map.empty, _reqId = 0}

data DefinitionRequest = DefinitionRequest
  { workDir :: FilePath,
    file :: FilePath,
    word :: String
  }
  deriving (Show, Generic)

data DefinitionResponse = DefinitionResponse
  { srcSpan :: Maybe SourceSpan,
    err :: Maybe String
  }
  deriving (Show, Generic, Eq)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

noDefintionFoundError :: Monad m => ExceptT String m a
noDefintionFoundError = throwE "No definition found"

noDefintionFoundErrorME :: MonadError String m => m a
noDefintionFoundErrorME = liftEither $ noDefintionFoundErrorE "No definition found"

noDefintionFoundErrorE :: (Monad m) => m (Either String a)
noDefintionFoundErrorE = runExceptT noDefintionFoundError

---

getExportedModule ::
  Package ->
  ModuleNameS ->
  Either String HsModuleP
getExportedModule Package {_ccpmodules = mods, _dependencies = ds} modN = do
  let modules = flip mapMaybe ds $ \pkg -> do
        guard $ modN `Map.member` _cabalPackageExportedModules pkg
        dependencyModules <- _cabalPackageName pkg `Map.lookup` mods
        modN `Map.lookup` dependencyModules
  case length modules of
    0 -> Left $ printf "no package seem to export" (show modN)
    1 -> Right $ head modules
    _ -> Left $ printf "multiple matches for %s: %s" (show modN) (show $ _name . HsModule._m <$> modules)

getAllImports :: HsModule -> (MonadLoggerIO m, MonadState Package m) => m Declarations
getAllImports m' = do
  let logTag = "get all imports for " <> show (_name m')
  Imports {importedDecls = iD, importedModules = iM, importedCDs = iCD} <- execWriterT $ haskellGetImports (_ast m')
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
  (a -> Map.Map PackageNameS (Map.Map ModuleNameS HsModuleP) -> CabalPackage -> Maybe a) ->
  (a -> Bool) ->
  a ->
  (MonadLoggerIO m, MonadState Package m) => m a
enrich0 f p d = do
  let logTag = "enrich"
  deps <- use dependencies
  mods <- use ccpmodules
  let xs = filter p $ mapMaybe (f d mods) deps
  case length xs of
    0 -> return d
    1 -> return $ head xs
    _ -> do
      logDebugNSS logTag $ printf "multiple matches for %s: %s" (show d) (show xs)
      return $ head xs

enrich :: HsModule -> (MonadLoggerIO m, MonadState Package m) => m Declarations
enrich mod = do
  importsD <- getAllImports mod
  importsE <- Map.elems <$> mapM (enrich0 enrichTryPackage hasNonEmptyOrig) (Declarations._decls importsD)
  importsCD <- Map.elems <$> mapM (enrich0 enrichTryPackageCDT (hasNonEmptyOrig . _cdtName)) (Declarations._dataTypes importsD)
  locals <- execWriterT $ haskellGetIdentifiers (_ast mod)
  return $ Declarations {_decls = asDeclsMap importsE <> Declarations._decls locals, _dataTypes = mapFrom (_declName . _cdtName) importsCD <> Declarations._dataTypes locals}

---

resolution :: Declarations -> Map.Map Identifier Declaration
resolution Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = Map.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> Map.elems (_cdtFields cd)) (Map.elems dts)
   in asDeclsMap $ ds' <> dts'

definition ::
  DefinitionRequest ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState ServerState m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf, word = w}) = do
  deps <- ultraZoom cabalPackages $ cabalDependencies wd

  logDebugNSS "definition" $ printf "deps list = %s" (show $ view cabalPackageName <$> deps)
  context . dependencies .= deps
  ultraZoom context Package.modules

  m <- ultraZoom context (parseModule emptyHsModule {_path = rf})
  m' <- resolution <$> ultraZoom context (enrich m)

  case w `Map.lookup` m' of
    Nothing -> noDefintionFoundErrorME
    Just d ->
      if hasNonEmptyOrig d
        then return $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
        else noDefintionFoundErrorME
