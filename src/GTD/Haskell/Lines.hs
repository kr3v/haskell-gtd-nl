{-# LANGUAGE DeriveGeneric #-}

module GTD.Haskell.Lines where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import qualified Data.ByteString.Char8 as BSC8
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import GTD.Haskell.Declaration (SourceSpan (..))
import System.FilePath (normalise)

data Line = Line
  { num :: Int,
    path :: BSC8.ByteString
  }
  deriving (Show, Generic, Eq, Ord)

instance FromJSON Line

instance ToJSON Line

instance Binary Line

type Lines = Map.Map Int Line

lineDirectiveBS :: BSC8.ByteString
lineDirectiveBS = BSC8.pack "#line"

dropDirectives :: BSC8.ByteString -> BSC8.ByteString
dropDirectives s = BSC8.unlines $ (\l -> if lineDirectiveBS `BSC8.isPrefixOf` l then BSC8.empty else l) <$> BSC8.lines s

-- #line 1 "/data/workspace/data/workspace/workspace/repos/personal/haskell-gtd/.repos/filepath-1.4.100.3/System/OsPath.hs"
buildMap :: BSC8.ByteString -> Map.Map Int Line
buildMap s = do
  let withNumbers = filter (\(_, l) -> lineDirectiveBS `BSC8.isPrefixOf` l) $ zip [1 ..] $ BSC8.lines s
  let directives = flip mapMaybe withNumbers $ \(n0, l) ->
        case BSC8.words l of
          (_ : n1 : p) -> Just $ (,) (n0 :: Int) Line {num = (read . BSC8.unpack $ n1) :: Int, path = BSC8.pack $ normalise $ drop 1 $ init $ BSC8.unpack $ BSC8.unwords p}
          _ -> Nothing
  Map.fromList directives

resolve :: Map.Map Int Line -> Int -> Maybe Line
resolve m n = n `Map.lookupLE` m <&> \(n0, l@Line {num = n1}) -> l {num = n - n0 + n1 - 1}

translate :: Map.Map Int Line -> SourceSpan -> SourceSpan
translate m d =
  case resolve m (_lineBegin d) of
    Just Line {path = p, num = n} -> d {_fileName = p, _lineBegin = n, _lineEnd = n}
    Nothing -> d