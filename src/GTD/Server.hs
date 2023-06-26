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
import Control.Monad.Trans.Writer (execWriterT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Distribution.Compat.Directory (listDirectory)
import GHC.Generics (Generic)
import GTD.Cabal (CabalPackage (..), ModuleNameS, cabalDeps, cabalPackageName, cabalPackagePath, cabalRead)
import GTD.Configuration (GTDConfiguration)
import GTD.Haskell.AST (Imports (..), haskellGetImports)
import qualified GTD.Haskell.AST as Declarations
import GTD.Haskell.Declaration (Declaration (_declSrcOrig), SourceSpan, hasNonEmptyOrig)
import GTD.Haskell.Enrich (enrichTryPackage)
import GTD.Haskell.Module (HsModule (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Haskell.Package (Package (..), ccpmodules, dependencies)
import qualified GTD.Haskell.Package as Package
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (deduplicateBy, logDebugNSS, logErrorNSS, ultraZoom)
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
  Either String HsModule
getExportedModule Package {_ccpmodules = mods, _dependencies = deps} modN = do
  let modules = flip mapMaybe deps $ \pkg -> do
        guard $ modN `Map.member` _cabalPackageExportedModules pkg
        dependencyModules <- _cabalPackageName pkg `Map.lookup` mods
        modN `Map.lookup` dependencyModules
  case length modules of
    0 -> Left $ printf "no package seem to export" (show modN)
    1 -> Right $ head modules
    _ -> Left $ printf "multiple matches for %s: %s" (show modN) (show $ _name <$> modules)

getAllImports :: HsModule -> (MonadLoggerIO m, MonadState Package m) => m [Declaration]
getAllImports mod = do
  let logTag = "get all imports for " <> show (_name mod)
  Imports {importedDecls = importsS, importedModules = importedMods} <- execWriterT $ haskellGetImports (_ast mod)
  ctx <- get
  let (errorsM, importsM) = partitionEithers $ getExportedModule ctx <$> importedMods
  forM_ errorsM $ \err -> logErrorNSS logTag err
  return $ importsS ++ concatMap (Map.elems . _exports) importsM

enrich0 :: Declaration -> (MonadLoggerIO m, MonadState Package m) => m Declaration
enrich0 d = do
  let logTag = "enrich"
  deps <- use dependencies
  mods <- use ccpmodules
  let xs = filter hasNonEmptyOrig $ mapMaybe (enrichTryPackage d mods) deps
  case length xs of
    0 -> return d
    1 -> return $ head xs
    _ -> do
      logDebugNSS logTag $ printf "multiple matches for %s: %s" (show d) (show xs)
      return $ head xs

enrich :: HsModule -> (MonadLoggerIO m, MonadState Package m) => m HsModule
enrich mod = do
  importsE <- getAllImports mod >>= mapM enrich0
  return $ flip execState mod $ do
    (HsModule.decls . Declarations.decls) %= (<>) (asDeclsMap importsE)

---

definition ::
  DefinitionRequest ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState ServerState m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf, word = w}) = do
  deps <- ultraZoom cabalPackages $ cabalDependencies wd

  logDebugNSS "definition" $ printf "deps list = %s" (show $ view cabalPackageName <$> deps)
  context . dependencies .= deps
  ultraZoom context Package.modules

  m <- ultraZoom context (parseModule emptyHsModule {_path = rf})
  m' <- ultraZoom context (enrich m)

  case w `Map.lookup` (Declarations._decls . HsModule._decls) m' of
    Nothing -> noDefintionFoundErrorME
    Just d ->
      if hasNonEmptyOrig d
        then return $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
        else noDefintionFoundErrorME
