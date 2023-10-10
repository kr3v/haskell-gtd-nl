{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Server.Usages where

import Control.Lens (makeLenses)
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as BSC8
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (listToMaybe)
import GHC.Generics (Generic)
import qualified GTD.Cabal.Types as Cabal (key, pKey)
import GTD.Configuration (Args (_powers), GTDConfiguration (..), Powers (..))
import GTD.Haskell.Declaration (SourceSpan (..))
import qualified GTD.Resolution.Cache.Usages as UsagesCache
import GTD.Server.Definition (DefinitionRequest (DefinitionRequest), cabalPackage, cabalPackage'unresolved'plusStoreInLocals, definition, package_)
import qualified GTD.Server.Definition as Definition (DefinitionRequest (..), DefinitionResponse(..))
import GTD.Server.Utils (x)
import GTD.State (MS)
import GTD.Utils (concatForM, deduplicate, logDebugNSS, stats, updateStatus)
import System.FilePath (normalise, splitDirectories, (</>))
import Text.Printf (printf)

data Request = Request
  { _origWorkDir :: FilePath,
    _workDir :: FilePath,
    _file :: FilePath,
    _word :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON Request

instance ToJSON Request

data Response = Response
  { _srcSpan :: [SourceSpan],
    _err :: Maybe String
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''Response)

instance FromJSON Response

instance ToJSON Response

usages :: Request -> (MS m, MonadError String m) => m Response
usages (Request {_origWorkDir = owd, _workDir = wd, _file = rf0, _word = w}) = do
  p <- asks $ _powers . _args
  logDebugNSS "usages" $ printf "p=%s" (show p)
  unless (_goToReferences_isEnabled p) $ throwError "Go to References is not enabled"
  let lim = _goToReferences_limit p

  let rf = normalise rf0
  let logTag = printf "usages: %s @ %s / %s" w rf wd

  r <- definition $ DefinitionRequest {Definition._origWorkDir = owd, Definition._workDir = wd, Definition._file = rf, Definition._word = w}
  forM_ (Definition._err r) throwError
  loc <- case Definition._srcSpan r of
    [] -> throwError $ printf "no definition found for %s" w
    [x] -> return x
    _ -> throwError $ printf "more than one definition found for %s?" w

  r <- asks _repos
  rs <- splitDirectories <$> asks _repos
  let locP = splitDirectories $ BSC8.unpack $ _fileName loc
  wdD <-
    maybe (throwError "cannot figure out stuff") return $
      if rs `isPrefixOf` locP
        then (r </>) <$> (listToMaybe =<< (rs `stripPrefix` locP))
        else return wd
  logDebugNSS logTag $ printf "rs=%s, locP=%s, wdD=%s" (show rs) (show locP) wdD
  cpkgsD <- cabalPackage wdD (BSC8.unpack $ _fileName loc)
  cpkgsO <- cabalPackage'unresolved'plusStoreInLocals owd
  forM_ cpkgsO package_

  logDebugNSS logTag $ printf "loc: %s" (show loc)
  let f = BSC8.unpack $ _fileName loc
  ss <- concatForM (listToMaybe cpkgsD) $ \cpkgD -> do
    logDebugNSS logTag $ printf "pkg: %s" (show $ Cabal.pKey . Cabal.key $ cpkgD)
    concatMap (x p loc) <$> UsagesCache.get cpkgsO cpkgD f
  liftIO stats
  updateStatus ""

  return Response {_srcSpan = deduplicate $ take lim $ ss, _err = Nothing}