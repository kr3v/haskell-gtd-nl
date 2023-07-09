{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server where

import Control.Exception (try)
import Control.Lens (At (at), use, (%=), (.=))
import Control.Monad (forM_, when, (>=>))
import Control.Monad.Except (ExceptT, MonadError, MonadIO (..), liftEither, runExceptT)
import Control.Monad.Logger (MonadLoggerIO, logDebug)
import Control.Monad.RWS (MonadReader (..), MonadState (..), asks)
import Control.Monad.State (evalStateT, execStateT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Writer (execWriterT)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BS
import Data.Either (fromRight, partitionEithers)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import GTD.Cabal (ModuleNameS)
import qualified GTD.Cabal as Cabal
import GTD.Configuration (GTDConfiguration (..))
import GTD.Haskell.AST (ClassOrData (..), Declarations (..), Imports (..), haskellGetIdentifiers, haskellGetImports)
import qualified GTD.Haskell.AST as Declarations
import GTD.Haskell.Declaration (Declaration (..), Identifier, SourceSpan, hasNonEmptyOrig)
import GTD.Haskell.Enrich (enrichTryModule, enrichTryModuleCDT)
import GTD.Haskell.Module (HsModule (..), HsModuleP (..), emptyHsModule, parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Haskell.Package (Context (..), Package (..), cExports, ccFindAt, ccFull, ccGet)
import qualified GTD.Haskell.Package as Package
import GTD.Haskell.Resolution (SchemeState (..), module'Dependencies, moduleR, scheme2, updateExports)
import GTD.Haskell.Utils (asDeclsMap)
import GTD.Utils (logDebugNSS, logErrorNSS, mapFrom, ultraZoom)
import System.FilePath.Posix ((</>))
import Text.Printf (printf)
import System.Directory (doesFileExist)

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
getExportedModule Package {_modules = mods} modN = do
  case modN `Map.lookup` mods of
    Nothing -> Left $ printf "no package seem to export %s" (show modN)
    Just m -> Right m

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
  importsE <- Map.elems <$> mapM (enrich0 (flip enrichTryModule) hasNonEmptyOrig) (Declarations._decls importsD)
  importsCD <- Map.elems <$> mapM (enrich0 (flip enrichTryModuleCDT) (hasNonEmptyOrig . _cdtName)) (Declarations._dataTypes importsD)
  locals <- execWriterT $ haskellGetIdentifiers (_ast m)
  return $ Declarations {_decls = asDeclsMap importsE <> Declarations._decls locals, _dataTypes = mapFrom (_declName . _cdtName) importsCD <> Declarations._dataTypes locals}

resolution :: Declarations -> Map.Map Identifier Declaration
resolution Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = Map.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> Map.elems (_cdtFields cd)) (Map.elems dts)
   in asDeclsMap $ ds' <> dts'

---

modules :: Package -> (MonadLoggerIO m) => m Package
modules pkg@Package {_cabalPackage = c} = do
  mods <- modules1 pkg c
  return pkg {Package._exports = Map.restrictKeys mods (Cabal._exports . Cabal._modules $ c), Package._modules = mods}

modules1 ::
  Package ->
  Cabal.PackageFull ->
  (MonadLoggerIO m) => m (Map.Map ModuleNameS HsModuleP)
modules1 pkg c = do
  modsO <- flip runReaderT c $ flip evalStateT (SchemeState Map.empty Map.empty) $ do
    scheme2 moduleR HsModule._name id module'Dependencies (Set.toList . Cabal._exports . Cabal._modules $ c)
  execStateT (forM_ modsO updateExports) (_modules pkg)

---

cabalFindAtCached ::
  FilePath ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m Cabal.PackageFull
cabalFindAtCached wd = do
  e <- use $ ccFindAt . at wd
  case e of
    Just d -> return d
    Nothing -> do
      d <- Cabal.findAt wd
      d' <- ultraZoom ccGet $ Cabal.full d
      ccFindAt %= Map.insert wd d'
      return d'

cabalFull ::
  Cabal.Package ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m Cabal.PackageFull
cabalFull pkg = do
  let k = Cabal.nameVersionP pkg
  e <- use $ ccFull . at k
  case e of
    Just d -> return d
    Nothing -> do
      d <- ultraZoom ccGet $ Cabal.full pkg
      ccFull %= Map.insert k d
      return d

cabalCacheGet :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
cabalCacheGet = do
  cfgP <- asks _ccGetPath
  cE :: Either IOError BS.ByteString <- liftIO $ try (BS.readFile cfgP)
  case cE of
    Left e -> logErrorNSS "cabalCacheGet" $ printf "readFile %s -> %s" cfgP (show e)
    Right c -> forM_ (decode c) (ccGet .=)

cabalCacheStore :: (MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m) => m ()
cabalCacheStore = do
  cfgP <- asks _ccGetPath
  cc <- use ccGet
  o :: Either IOError () <- liftIO $ try $ BS.writeFile cfgP $ encode cc
  case o of
    Left e -> logErrorNSS "cabalCacheStore" $ printf "writeFile %s -> %s" cfgP (show e)
    Right _ -> return ()

---

persistenceGet :: Cabal.PackageFull -> FilePath -> (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m, FromJSON a) => m (Maybe a)
persistenceGet cPkg f = do
  let p = (Cabal._path . Cabal._fpackage $ cPkg) </> f
  rJ :: Either IOError (Maybe a) <- liftIO (try $ decode <$> BS.readFile p)
  let r = fromRight Nothing rJ
  logDebugNSS "persistence get" $ printf "%s / %s -> %s" (show $ Cabal.nameVersionF cPkg) p (show $ isJust r)
  return r

persistenceExists :: Cabal.Package -> (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m Bool
persistenceExists cPkg = do
  let p = Cabal._path cPkg </> "exports.json"
  r <- liftIO $ doesFileExist p
  logDebugNSS "package cached exists" $ printf "%s, %s -> %s" (show $ Cabal.nameVersionP cPkg) p (show r)
  return r

packagePersistenceGet ::
  Cabal.PackageFull ->
  (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m (Maybe Package)
packagePersistenceGet cPkg = do
  modulesJ <- persistenceGet cPkg "modules.json"
  exportsJ <- persistenceGet cPkg "exports.json"
  let p = Package cPkg <$> modulesJ <*> exportsJ
  logDebugNSS "package cached get" $ printf "%s -> %s" (show $ Cabal.nameVersionF cPkg) (show $ isJust p)
  return p

packageCachedPut ::
  Cabal.PackageFull ->
  Package ->
  (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m ()
packageCachedPut cPkg pkg = do
  let root = Cabal._path . Cabal._fpackage $ cPkg
      modulesP = root </> "modules.json"
      exportsP = root </> "exports.json"
  liftIO $ BS.writeFile modulesP $ encode $ Package._modules pkg
  liftIO $ BS.writeFile exportsP $ encode $ Package._exports pkg
  logDebugNSS "package cached put" $ printf "%s -> (%d, %d)" (show $ Cabal.nameVersionF cPkg) (length $ Package._modules pkg) (length $ Package._exports pkg)

packageCachedGet ::
  Cabal.PackageFull ->
  (MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m (Maybe Package)
packageCachedGet cPkg = do
  let k = Cabal.nameVersionF cPkg
  c <- use $ cExports . at k
  case c of
    Just es -> return $ Just Package {_cabalPackage = cPkg, _modules = Map.empty, Package._exports = es}
    Nothing -> do
      eM <- persistenceGet cPkg "exports.json"
      case eM of
        Nothing -> return Nothing
        Just e -> do
          cExports %= Map.insert k e
          return $ Just Package {_cabalPackage = cPkg, _modules = Map.empty, Package._exports = e}

package0 ::
  Cabal.PackageFull ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m ()
package0 cPkg = do
  pkgM <- packageCachedGet cPkg
  case pkgM of
    Just _ -> return ()
    Nothing -> do
      depsC <- catMaybes <$> mapM (cabalFull >=> packageCachedGet) (Cabal._dependencies cPkg)
      let deps = foldr (<>) Map.empty $ Package._exports <$> depsC
      pkg <- modules $ Package {_cabalPackage = cPkg, Package._modules = deps, Package._exports = Map.empty}
      packageCachedPut cPkg pkg

package ::
  Cabal.PackageFull ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadState Context m, MonadReader GTDConfiguration m) => m (Maybe Package)
package cPkg0 = do
  m <- packagePersistenceGet cPkg0
  case m of
    Just p -> return $ Just p
    Nothing -> do
      pkgsO <- flip evalStateT (SchemeState Map.empty Map.empty) $ do
        scheme2 (\cPkg -> do b <- persistenceExists cPkg; if b then return Nothing else ((Just <$>) . cabalFull) cPkg) Cabal.nameVersionF Cabal.nameVersionP (return . Cabal._dependencies) (Cabal._dependencies cPkg0)
      forM_ pkgsO package0
      package0 cPkg0
      packagePersistenceGet cPkg0

definition ::
  DefinitionRequest ->
  (MonadBaseControl IO m, MonadLoggerIO m, MonadReader GTDConfiguration m, MonadState Context m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf, word = w}) = do
  cPkg <- cabalFindAtCached wd
  pkgM <- package cPkg
  pkg <- maybe noDefintionFoundErrorME return pkgM

  m <- parseModule emptyHsModule {_path = rf}
  m' <- resolution <$> evalStateT (enrich m) pkg

  ccGC <- use $ ccGet . Cabal.changed
  when ccGC $ do
    cabalCacheStore

  case w `Map.lookup` m' of
    Nothing -> noDefintionFoundErrorME
    Just d ->
      if hasNonEmptyOrig d
        then return $ DefinitionResponse {srcSpan = Just $ _declSrcOrig d, err = Nothing}
        else noDefintionFoundErrorME
