{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Resolution.Module.Multi (resolution, figureOutExports0, collectUsages) where

import Control.Monad (forM_, when)
import Control.Monad.Logger (MonadLoggerIO, NoLoggingT (runNoLoggingT))
import Control.Monad.RWS (MonadReader (..), MonadWriter (..))
import Control.Monad.State.Lazy (execStateT, modify)
import Control.Monad.Trans.Writer (execWriterT)
import qualified Data.ByteString.Char8 as BSC8
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HMap
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GTD.Cabal.Types (ModuleNameS)
import GTD.Configuration (GTDConfiguration)
import GTD.Haskell.Declaration (ClassOrData (..), Declaration (..), Declarations (..), IdentifierWithUsageLocation (..), Module (..), ModuleImportType (..), SourceSpan (..), asDeclsHMap, emptySourceSpan)
import GTD.Haskell.Module (HsModule (..), HsModuleData (..), HsModuleP (..), HsModuleParams (..), _name)
import qualified GTD.Haskell.Module as HsModule
import qualified GTD.Resolution.Cache.Resolution as ResolutionCache
import GTD.Resolution.Module.Types (UsagesInFileMap (..), UsagesMap)
import GTD.Resolution.Module.Utils (resolution'simplify, resolution'word'parsed'b)
import GTD.Utils (mapHFrom, withoutKeys)

resolution ::
  HMap.HashMap ModuleNameS HsModuleP ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (HMap.HashMap ModuleNameS Declarations)
resolution sM m = do
  x <- runNoLoggingT $ ResolutionCache.get m
  case x of
    Just r -> return r
    Nothing -> do
      r <- resolution'direct sM m
      ResolutionCache.put m r
      return r

-- resolution'direct returns a map that allows 'resolving' 'words' for the given module
resolution'direct ::
  HMap.HashMap ModuleNameS HsModuleP ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (HMap.HashMap ModuleNameS Declarations)
resolution'direct sM m = flip execStateT HMap.empty $ do
  let locals = _locals . _info $ m
      name = _name m

  forM_ (_imports . _info $ m) $
    \Module {_mName = k, _mType = mt, _mAllowNoQualifier = mnq, _mQualifier = mq, _mDecls = md, _mCDs = mc} ->
      forM_ (HMap.lookup k sM) $ \c -> do
        let o = emptySourceSpan {_fileName = BSC8.pack . HsModule._mPath . HsModule._ometadata $ c, _colBegin = 1, _lineBegin = 1}
        modify $ HMap.insertWith (<>) (k <> BSC8.pack "*") mempty {_decls = HMap.singleton mempty Declaration {_declModule = k, _declName = mempty, _declSrcOrig = o, _declSrcOthers = []}}
        let stuff = case mt of
              All -> _exports c
              EverythingBut -> Declarations {_decls = withoutKeys (_decls $ _exports c) (_declName <$> md), _dataTypes = withoutKeys (_dataTypes $ _exports c) (_declName . _cdtName <$> mc)}
              Exactly -> Declarations {_decls = HMap.intersection (_decls $ _exports c) (asDeclsHMap md), _dataTypes = HMap.intersection (_dataTypes $ _exports c) (mapHFrom (_declName . _cdtName) mc)}
        modify $ HMap.insertWith (<>) mq stuff
        when mnq $ modify $ HMap.insertWith (<>) mempty stuff
  modify $ HMap.insertWith (<>) name locals
  modify $ HMap.insertWith (<>) mempty locals

---

-- figureOutExports0 returns a list of exported identifiers for a given module and its 'resolution' map
figureOutExports0 ::
  HMap.HashMap ModuleNameS Declarations ->
  HsModule ->
  (MonadLoggerIO m, MonadReader GTDConfiguration m) => m (HsModuleP, HMap.HashMap ModuleNameS HsModuleP -> HMap.HashMap ModuleNameS HsModuleP)
figureOutExports0 liM m = do
  r <-
    execWriterT $
      if _isImplicitExportAll . _params $ m
        then tell $ _locals . _info $ m
        else forM_ (Map.toList $ _exports0 . _info $ m) $ \(k, e) -> do
          let n = _name m
          if _mType e == All
            then
              if k == n
                then tell $ _locals . _info $ m
                else forM_ (HMap.lookup k liM) $ \c -> tell c
            else forM_ (HMap.lookup k liM) $ \c -> do
              let eD = Declarations {_decls = HMap.intersection (_decls c) (asDeclsHMap $ _mDecls e), _dataTypes = HMap.intersection (_dataTypes c) (mapHFrom (_declName . _cdtName) $ _mCDs e)}
              tell eD

  let x = HsModuleP {HsModule._exports = r, HsModule._ometadata = _metadata m}
  return (x, HMap.insert (_name m) x)

---

-- collectUsages figures out what identifiers are used in a given module, so that they can have the 'usages' code lens
collectUsages ::
  HsModule ->
  HMap.HashMap ModuleNameS Declarations ->
  (UsagesMap -> UsagesMap)
collectUsages m liM = do
  let ids = _identifierUsages . _info $ m
  let liMb = resolution'simplify <$> liM
  foldr (.) id $
    flip mapMaybe ids \(IdentifierUsage {_iuModule = mn, _iuName = n, _iuType = iu, _iuSourceSpan = is}) ->
      resolution'word'parsed'b liMb mn n <&> \r ->
        HMap.insertWith (HMap.unionWith (<>)) (_fileName r) $ HMap.singleton iu (UsagesInFileMap $ HMap.singleton r [is])
