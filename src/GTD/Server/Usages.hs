{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server.Usages where

import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HMap
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Generics (Generic)
import qualified GTD.Cabal.FindAt as CabalCache (findAt)
import qualified GTD.Cabal.Types as Cabal (key, pKey)
import GTD.Configuration (Args (_powers), GTDConfiguration (..), Powers (_goToReferences_isEnabled, _goToReferences_limit))
import GTD.Haskell.Declaration (SourceSpan (..))
import qualified GTD.Resolution.Cache.Usages as UsagesCache
import GTD.Server.Definition (DefinitionRequest (DefinitionRequest), cabalPackage, definition)
import qualified GTD.Server.Definition as Definition (DefinitionRequest (..), err, srcSpan)
import GTD.State (MS)
import GTD.Utils (concatForM, deduplicate, logDebugNSS, stats, updateStatus)
import System.FilePath (normalise, splitDirectories, (</>))
import Text.Printf (printf)

data Request = Request
  { origWorkDir :: FilePath,
    workDir :: FilePath,
    file :: FilePath,
    word :: String
  }
  deriving (Show, Eq, Generic)

instance FromJSON Request

instance ToJSON Request

data Response = Response
  { srcSpan :: [SourceSpan],
    err :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response

instance ToJSON Response

usages :: Request -> (MS m, MonadError String m) => m Response
usages (Request {origWorkDir = owd, workDir = wd, file = rf0, word = w}) = do
  e <- asks $ _goToReferences_isEnabled . _powers . _args
  unless e $ throwError "Go to References is not enabled"
  lim <- asks $ _goToReferences_limit . _powers . _args

  let rf = normalise rf0
  let logTag = printf "usages: %s @ %s / %s" w rf wd

  r <- definition $ DefinitionRequest {Definition.origWorkDir = owd, Definition.workDir = wd, Definition.file = rf, Definition.word = w}
  forM_ (Definition.err r) throwError
  loc <- case Definition.srcSpan r of
    [] -> throwError $ printf "no definition found for %s" w
    [x] -> return x
    _ -> throwError $ printf "more than one definition found for %s?" w

  r <- asks _repos
  rs <- splitDirectories <$> asks _repos
  let locP = splitDirectories $ BSC8.unpack $ sourceSpanFileName loc
  wdD <-
    maybe (throwError "cannot figure out stuff") return $
      if rs `isPrefixOf` locP
        then (r </>) <$> (listToMaybe =<< (rs `stripPrefix` locP))
        else return wd
  logDebugNSS logTag $ printf "rs=%s, locP=%s, wdD=%s" (show rs) (show locP) wdD
  cpkgsD <- cabalPackage wdD (BSC8.unpack $ sourceSpanFileName loc)
  cpkgsO <- CabalCache.findAt owd

  ss <- fmap concat $ do
    logDebugNSS logTag $ printf "loc: %s" (show loc)
    let f = BSC8.unpack $ sourceSpanFileName loc
    concatForM (listToMaybe cpkgsD) $ \cpkgD -> do
      logDebugNSS logTag $ printf "pkg: %s" (show $ Cabal.pKey . Cabal.key $ cpkgD)
      mapMaybe (HMap.lookup loc) <$> UsagesCache.get cpkgsO cpkgD f
  liftIO stats
  updateStatus ""

  return Response {srcSpan = deduplicate $ take lim $ ss, err = Nothing}