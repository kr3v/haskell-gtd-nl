{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- this set of functions is responsible for parsing a single module (no inter-module resolution happens here) in a given Cabal package
module GTD.Resolution.Module.Single (resolve, module'Dependencies, moduleR) where

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Cont (forM, forM_)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (MonadReader (ask))
import Control.Monad.Trans.Except (runExceptT)
import Data.Either (partitionEithers)
import qualified Data.Set as Set
import Distribution.ModuleName (fromString, toFilePath)
import GTD.Cabal.Types (ModuleNameS)
import qualified GTD.Cabal.Types as Cabal (Package (_modules, _name, _root), PackageModules (_allKnownModules, _srcDirs), PackageWithResolvedDependencies, key, _exports)
import GTD.Haskell.Declaration (allImportedModules)
import GTD.Haskell.Module (HsModule (..), HsModuleData (..), HsModuleMetadata (HsModuleMetadata), parseModule)
import qualified GTD.Haskell.Module as HsModule
import GTD.Utils (logDebugNSS, logErrorNSS)
import System.FilePath (normalise, (</>))
import Text.Printf (printf)

resolve :: FilePath -> FilePath -> ModuleNameS -> FilePath
resolve repoRoot srcDir moduleName = normalise $ repoRoot </> srcDir </> ((toFilePath . fromString $ moduleName) ++ ".hs")

module'Dependencies :: HsModule -> [ModuleNameS]
module'Dependencies m = filter (HsModule._name m /=) (allImportedModules . _imports . HsModule._info $ m)

module'2 :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m [Either String HsModule]
module'2 p m = do
  let root = Cabal._root p
  let srcDirs = Cabal._srcDirs . Cabal._modules $ p
  forM srcDirs $ \srcDir -> runExceptT $ do
    let path = resolve root srcDir m
    logDebugNSS "module'2" $ printf "resolve(%s, %s, %s) -> %s" root srcDir m path
    let cm = HsModuleMetadata {HsModule._mPackage = Cabal._name p, HsModule._mName = m, HsModule._mPath = path, HsModule._mPkgK = Cabal.key p}
    parseModule cm
      `catchError` \e1 ->
        parseModule (cm {HsModule._mPath = path ++ "c"})
          `catchError` \e2 -> throwError $ printf "error parsing module %s/%s (hs,hsc): (%s, %s)" (show srcDir) (show m) (show e1) (show e2)

module'1 :: Cabal.PackageWithResolvedDependencies -> ModuleNameS -> (MonadLoggerIO m) => m ([String], Maybe HsModule)
module'1 p mn = do
  (errs, ms) <- partitionEithers <$> module'2 p mn
  let shouldBePresent = Set.member mn (Cabal._allKnownModules . Cabal._modules $ p)
  return $ case ms of
    [] -> (if shouldBePresent then errs else [], Nothing)
    [m] -> (errs, Just m)
    _ -> (errs ++ [printf "multiple modules found: %s" (show $ liftA2 (,) HsModule._name HsModule._path <$> ms)], Nothing)

moduleR :: ModuleNameS -> (MonadLoggerIO m, MonadReader Cabal.PackageWithResolvedDependencies m) => m (Maybe HsModule)
moduleR m = do
  p <- ask
  (es, cm) <- module'1 p m
  forM_ es $ logErrorNSS "parse module in package"
  return cm
