{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server.Usages where

import Control.Monad (forM_, unless, when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS (asks)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.HashMap.Strict as HMap
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Generics (Generic)
import qualified GTD.Cabal.Types as Cabal (key, pKey)
import GTD.Configuration (Args (_powers), GTDConfiguration (_args), Powers (_isGoToReferencesEnabled))
import GTD.Haskell.Declaration (SourceSpan (..))
import GTD.Resolution.Cache (pGetU)
import GTD.Server.Definition (DefinitionRequest (DefinitionRequest), cabalPackage, definition)
import qualified GTD.Server.Definition as Definition
import GTD.State (MS)
import GTD.Utils (concatForM, deduplicate, logDebugNSS, stats, updateStatus)
import System.FilePath (normalise)
import Text.Printf (printf)

data Request = Request
  { workDir :: FilePath,
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
usages (Request {workDir = wd, file = rf0, word = w}) = do
  e <- asks $ _isGoToReferencesEnabled . _powers . _args
  unless e $ throwError "Go to References is not enabled"

  let rf = normalise rf0
  let logTag = printf "usages: %s @ %s / %s" w rf wd

  updateStatus $ printf "resolving Cabal package for %s" wd
  cPkgs <- cabalPackage wd rf

  r <- definition $ DefinitionRequest {Definition.workDir = wd, Definition.file = rf, Definition.word = w}
  forM_ (Definition.err r) throwError
  when (null (Definition.srcSpan r)) $ throwError $ printf "no definition found for %s" w
  when (length (Definition.srcSpan r) > 1) $ throwError $ printf "more than one definition found for %s?" w

  ss <- fmap concat $ concatForM (Definition.srcSpan r) $ \loc -> do
    logDebugNSS logTag $ printf "loc: %s" (show loc)
    let f = BSC8.unpack $ sourceSpanFileName loc
    concatForM (listToMaybe cPkgs) $ \cPkg -> do
      logDebugNSS logTag $ printf "pkg: %s" (show $ Cabal.pKey . Cabal.key $ cPkg)
      mapMaybe (HMap.lookup loc) <$> pGetU cPkg f
  liftIO stats
  updateStatus ""
  return Response {srcSpan = deduplicate $ take 64 $ ss, err = Nothing}