{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GTD.Server.CodeLens.LocalUsages where

import Control.Monad (forM, forM_)
import Control.Monad.Except (MonadError (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict as HMap
import GHC.Generics (Generic)
import qualified GTD.Resolution.Cache.Usages as UsagesCache
import GTD.Resolution.Types (UsagesInFileMap)
import GTD.Server.Definition (cabalPackage, findAtF, package_)
import GTD.State (MS)
import GTD.Utils (TupleList, deduplicate, updateStatus)
import System.FilePath (normalise)

data Request = Request
  { workDir :: FilePath,
    file :: FilePath
  }
  deriving (Show, Eq, Generic)

instance FromJSON Request

instance ToJSON Request

data Response = Response
  { srcSpan :: TupleList UsagesInFileMap,
    err :: Maybe String
  }
  deriving (Show, Eq, Generic)

instance FromJSON Response

instance ToJSON Response

usages :: Request -> (MS m, MonadError String m) => m Response
usages (Request {workDir = wd, file = rf0}) = do
  let rf = normalise rf0

  ps <- findAtF wd
  forM_ ps package_

  cpkgsO <- cabalPackage wd rf
  fs <- foldr (HMap.unionWith (<>)) HMap.empty . concat <$> forM cpkgsO (flip UsagesCache.getAll rf)
  updateStatus ""

  return Response {srcSpan = fmap deduplicate <$> HMap.toList fs, err = Nothing}