{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server.Definition where

import Control.Lens (use, (%=), (.=))
import Control.Monad (forM, forM_, join, unless, void, when)
import Control.Monad.Except (MonadError (..), MonadIO (..))
import Control.Monad.Logger (MonadLoggerIO)
import Control.Monad.RWS (gets)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Cache.LRU as LRU
import Data.Foldable (foldlM)
import Data.List (isPrefixOf, isSuffixOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import GHC.Generics (Generic)
import qualified GTD.Cabal.Dependencies as Cabal (fullS)
import qualified GTD.Cabal.FindAt as CabalCache (findAt)
import GTD.Cabal.Types (PackageWithResolvedDependencies, PackageWithUnresolvedDependencies)
import qualified GTD.Cabal.Types as Cabal (Dependency, Designation (..), DesignationType (..), Package (..), PackageModules (..), PackageWithResolvedDependencies, PackageWithUnresolvedDependencies, key, pKey)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), Identifier, SourceSpan (..), asDeclsMap, emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleMetadata (..), emptyHsModule, emptyMetadata)
import qualified GTD.Haskell.Parser.GhcLibParser as GHC
import qualified GTD.Resolution.Cache as PackageCache
import GTD.Resolution.Package (package'resolution'withDependencies'forked)
import GTD.Resolution.Types (Package (..))
import GTD.State (Context (..), MS, cLocalPackages, cResolution)
import GTD.Utils (logDebugNSS, maybeToMaybeT, peekM, stats, updateStatus)
import System.FilePath (normalise, (</>))
import Text.Printf (printf)

package ::
  Cabal.PackageWithResolvedDependencies ->
  (MS m) => m (Maybe Package)
package cPkg0 =
  PackageCache.pGet cPkg0 >>= \case
    Just p -> return $ Just p
    Nothing -> do
      package'resolution'withDependencies'forked cPkg0
      PackageCache.pGet cPkg0

package_ :: Cabal.Package a -> (MS m) => m ()
package_ cPkg0 =
  PackageCache.pExists cPkg0 >>= \case
    True -> return ()
    False -> package'resolution'withDependencies'forked cPkg0

cabalPackage'unresolved'plusStoreInLocals :: FilePath -> (MS m, MonadError String m) => m [Cabal.PackageWithUnresolvedDependencies]
cabalPackage'unresolved'plusStoreInLocals f = peekM cabalPackage'contextWithLocals (CabalCache.findAt f)

cabalPackage'contextWithLocals :: (MS m) => [Cabal.PackageWithUnresolvedDependencies] -> m ()
cabalPackage'contextWithLocals cPkgsU = do
  logDebugNSS "cabalPackage'contextWithLocals" $ printf "cPkgsU = %s" (show $ Cabal.pKey . Cabal.key <$> cPkgsU)
  let libs = filter (\p -> (Cabal._desType . Cabal._designation $ p) == Cabal.Library) cPkgsU
  cLocalPackages .= mempty
  forM_ libs $ \cPkg -> do
    cLocalPackages %= Map.insertWith (<>) (Cabal._name cPkg, Cabal._desName . Cabal._designation $ cPkg) (Map.singleton (Cabal._version cPkg) cPkg)
  l <- use cLocalPackages
  logDebugNSS "cabalPackage'contextWithLocals" $ printf "cLocalPackages = %s" (show ((\(k, vs) -> (\(v, p) -> (k, v, Cabal._designation p)) <$> Map.toList vs) <$> Map.toList l))

cabalPackage'resolve :: (MS m) => [Cabal.Package Cabal.Dependency] -> m [PackageWithResolvedDependencies]
cabalPackage'resolve = mapM Cabal.fullS

findAtF :: FilePath -> (MS m, MonadError String m) => m [PackageWithResolvedDependencies]
findAtF wd = cabalPackage'unresolved'plusStoreInLocals wd >>= cabalPackage'resolve

cabalPackage :: FilePath -> FilePath -> (MS m, MonadError String m) => m [PackageWithUnresolvedDependencies]
cabalPackage wd rf = do
  cPkgsU <- cabalPackage'unresolved'plusStoreInLocals wd
  if not (".hs" `isSuffixOf` rf) && not (".lhs" `isSuffixOf` rf) && not (".hs-boot" `isSuffixOf` rf) && not (".hsc" `isSuffixOf` rf)
    then return cPkgsU
    else do
      let srcDirs p = (\d -> normalise $ Cabal._root p </> d) <$> (Cabal._srcDirs . Cabal._modules $ p)
          cPkgs = filter (any (`isPrefixOf` rf) . srcDirs) cPkgsU
      when (null cPkgs) $ throwError $ "cannot find a cabal 'item' with source directory that owns file " ++ rf
      forM_ cPkgs $ \cPkg -> do
        e <- PackageCache.pExists cPkg
        unless e $ void $ package_ cPkg
      logDebugNSS "cabalPackage" $ printf "cPkgs = %s" (show $ Cabal.pKey . Cabal.key <$> cPkgs)
      return cPkgs

---

data DefinitionRequest = DefinitionRequest
  { workDir :: FilePath,
    file :: FilePath,
    word :: String
  }
  deriving (Show, Generic)

data DefinitionResponse = DefinitionResponse
  { srcSpan :: [SourceSpan],
    err :: Maybe String
  }
  deriving (Show, Generic, Eq)

instance ToJSON DefinitionRequest

instance FromJSON DefinitionRequest

instance ToJSON DefinitionResponse

instance FromJSON DefinitionResponse

resolution'simplify :: Declarations -> Map.Map Identifier Declaration
resolution'simplify Declarations {_decls = ds, _dataTypes = dts} =
  let ds' = Map.elems ds
      dts' = concatMap (\cd -> [_cdtName cd] <> Map.elems (_cdtFields cd)) (Map.elems dts)
   in asDeclsMap $ ds' <> dts'

resolution'qualified ::
  Map.Map String Declarations ->
  String ->
  (MonadLoggerIO m, MonadError String m) => MaybeT m SourceSpan
resolution'qualified rm w = do
  y <- maybeToMaybeT $ w `Map.lookup` rm
  case Map.elems $ resolution'simplify y of
    (d : _) -> return $ emptySourceSpan {sourceSpanFileName = sourceSpanFileName . _declSrcOrig $ d, sourceSpanStartColumn = 1, sourceSpanStartLine = 1}
    _ -> throwError "given word is a known module, but it has no declarations"

resolution'word ::
  Map.Map String Declarations ->
  String ->
  (MonadLoggerIO m, MonadError String m) => MaybeT m SourceSpan
resolution'word rm w = do
  let (q, w') = fromMaybe ("", w) $ GHC.identifier w
  let look q1 w1 = join $ do
        mQ <- Map.lookup q1 rm
        d <- Map.lookup w1 $ resolution'simplify mQ
        return $ Just $ _declSrcOrig d
  let cases = if w == w' then [("", w)] else [(q, w'), ("", w)]
  casesM <- forM cases $ \(q1, w1) -> do
    let r = look q1 w1
    logDebugNSS "definition" $ printf "%s -> `%s`.`%s` -> %s" w q1 w1 (show r)
    return r
  case catMaybes casesM of
    (x : _) -> return x
    _ -> MaybeT $ return Nothing

resolution :: (MonadLoggerIO m, MonadError String m) => Map.Map String Declarations -> String -> m (Maybe SourceSpan)
resolution rm w = do
  let opts =
        [ resolution'qualified rm (w ++ "*"),
          resolution'qualified rm w,
          resolution'word rm w
        ]
  foldlM (\a b -> case a of Just _ -> return a; Nothing -> b) Nothing $ runMaybeT <$> opts

definition ::
  DefinitionRequest ->
  (MS m, MonadError String m) => m DefinitionResponse
definition (DefinitionRequest {workDir = wd, file = rf0, word = w}) = do
  let rf = normalise rf0
  updateStatus $ printf "resolving Cabal package for %s" wd
  cPkgs <- cabalPackage wd rf

  ss <- forM cPkgs $ \cPkg -> do
    let pn = show $ Cabal.pKey . Cabal.key $ cPkg
    updateStatus $ printf "fetching 'resolution' map for %s in %s" rf pn

    let k = (Cabal.key cPkg, rf)
    (l, mL) <- gets $ LRU.lookup k . _cResolution
    cResolution .= l
    resM <- case mL of
      Just x -> return x
      Nothing -> do
        let m = emptyHsModule {_metadata = emptyMetadata {_mPath = rf, _mPkgK = Cabal.key cPkg}}
        r <- PackageCache.resolution'get m
        cResolution %= LRU.insert k r
        return r

    r <- case resM of
      Nothing -> return Nothing
      Just rm -> do
        updateStatus $ printf "figuring out what `%s` is in %s" w pn
        resolution rm w
    logDebugNSS "definition" $ printf "resolution %s %s (resM = %s) = %s" w pn (show $ isJust resM) (show r)
    return r
  liftIO stats
  updateStatus ""
  return DefinitionResponse {srcSpan = catMaybes ss, err = Nothing}
