{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module GTD.Server.CodeLens.LocalUsages where

import Control.Lens (makeLenses)
import Control.Monad (forM_, unless)
import Control.Monad.Except (MonadError (..))
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HMap
import GHC.Generics (Generic)
import GTD.Configuration (Args (..), GTDConfiguration (..), Powers (..))
import qualified GTD.Resolution.Cache.Usages as UsagesCache
import GTD.Resolution.Module.Types (UsagesInFileMap (..), UsagesInFileMapM)
import GTD.Server.Definition (cabalPackage, findAtF, package_)
import GTD.Server.Utils (x1)
import GTD.State (MS)
import GTD.Utils (TupleList, concatMapM, deduplicate, updateStatus)
import System.FilePath (normalise)

data Request = Request
  { _workDir :: FilePath,
    _file :: FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON Request

instance ToJSON Request

$(makeLenses ''Request)

data Response = Response
  { _srcSpan :: TupleList UsagesInFileMapM,
    _err :: Maybe String
  }
  deriving (Show, Eq, Generic)

$(makeLenses ''Response)

instance FromJSON Response

instance ToJSON Response

usages :: Request -> (MS m, MonadError String m) => m Response
usages (Request {_workDir = wd, _file = rf0}) = do
  p <- asks $ _powers . _args
  unless (_goToReferences_lens_isEnabled p) $ throwError "Go to References is not enabled"

  let rf = normalise rf0

  ps <- findAtF wd
  forM_ ps package_

  cpkgsO <- cabalPackage wd rf
  fs <- concatMap (x1 p) <$> concatMapM (flip UsagesCache.getAll rf) cpkgsO
  updateStatus ""
  let z = foldr (HMap.unionWith (<>) . _m) HMap.empty fs

  return Response {_srcSpan = fmap deduplicate <$> HMap.toList z, _err = Nothing}