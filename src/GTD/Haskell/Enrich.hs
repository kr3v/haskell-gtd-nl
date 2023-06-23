{-# LANGUAGE RankNTypes #-}

module GTD.Haskell.Enrich where

import Control.Monad.Logger (MonadLogger)
import Control.Monad.RWS (guard)
import Control.Monad.Trans.Maybe (MaybeT)
import qualified Data.Map as Map
import GTD.Cabal (CabalPackage (_cabalPackageExportedModules, _cabalPackageName), ModuleNameS, PackageNameS)
import GTD.Haskell.Declaration (Declaration (..), Identifier (..))
import GTD.Haskell.Module (HsModule (_exports))
import GTD.Utils (logDebugNSS, maybeToMaybeT)
import Text.Printf (printf)

enrichTryPackage ::
  Declaration ->
  Map.Map PackageNameS (Map.Map ModuleNameS HsModule) ->
  CabalPackage ->
  (MonadLogger m) => MaybeT m Declaration
enrichTryPackage d mods dep = do
  guard $ _declModule d `Map.member` _cabalPackageExportedModules dep
  dependencyModules <- maybeToMaybeT $ _cabalPackageName dep `Map.lookup` mods
  enrichTryModule d dependencyModules

enrichTryModule ::
  Declaration ->
  Map.Map ModuleNameS HsModule ->
  (MonadLogger m) => MaybeT m Declaration
enrichTryModule orig moduleDecls = do
  let logTag = "enrich by module"
  mod <- maybeToMaybeT $ _declModule orig `Map.lookup` moduleDecls
  mDecl <- maybeToMaybeT $ Map.lookup (Identifier $ _declName orig) (_exports mod)
  logDebugNSS logTag $ printf "enrich: updating %s with %s" (show orig) (show mDecl)
  return $ orig {_declSrcOrig = _declSrcOrig mDecl}
