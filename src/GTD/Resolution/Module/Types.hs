{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module GTD.Resolution.Module.Types where

import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value, object, withObject, (.:))
import Data.Aeson.Key (fromString)
import Data.Aeson.Types (Parser)
import Data.Binary (Binary)
import qualified Data.HashMap.Strict as HMap
import GHC.Generics (Generic)
import GTD.Haskell.Declaration (SourceSpan, SourceSpanFileName, UsageType)

type UsagesMap = HMap.HashMap SourceSpanFileName UsagesPerTypeInFileMap

type UsagesPerTypeInFileMap = HMap.HashMap UsageType UsagesInFileMap

type UsagesInFileMapM = HMap.HashMap SourceSpan [SourceSpan]

newtype UsagesInFileMap = UsagesInFileMap {_m :: HMap.HashMap SourceSpan [SourceSpan]} deriving (Show, Generic, Eq)

instance Semigroup UsagesInFileMap where
  (<>) :: UsagesInFileMap -> UsagesInFileMap -> UsagesInFileMap
  (<>) (UsagesInFileMap a) (UsagesInFileMap b) = UsagesInFileMap $ HMap.unionWith (<>) a b

instance Monoid UsagesInFileMap where
  mempty :: UsagesInFileMap
  mempty = UsagesInFileMap HMap.empty

  mappend :: UsagesInFileMap -> UsagesInFileMap -> UsagesInFileMap
  mappend = (<>)

instance ToJSON UsagesInFileMap where
  toJSON :: UsagesInFileMap -> Value
  toJSON (UsagesInFileMap o) = object [fromString "_usages" .= HMap.toList o]

instance FromJSON UsagesInFileMap where
  parseJSON :: Value -> Parser UsagesInFileMap
  parseJSON = withObject "UsagesMap" $ \v -> do
    uL <- v .: fromString "_usages"
    return $ UsagesInFileMap $ HMap.fromList uL

instance Binary UsagesInFileMap