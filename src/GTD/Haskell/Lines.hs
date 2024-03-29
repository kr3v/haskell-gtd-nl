{-# LANGUAGE DeriveGeneric #-}

module GTD.Haskell.Lines where

import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Functor ((<&>))
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Generics (Generic)
import System.FilePath (normalise)

data Line = Line
  { num :: Int,
    path :: FilePath
  }
  deriving (Show, Generic, Eq, Ord)

instance FromJSON Line

instance ToJSON Line

instance Binary Line

type Lines = Map.Map Int Line

dropDirectives :: String -> String
dropDirectives s = unlines $ (\l -> if "#line" `isPrefixOf` l then "" else l) <$> lines s

-- #line 1 "/data/workspace/data/workspace/workspace/repos/personal/haskell-gtd/.repos/filepath-1.4.100.3/System/OsPath.hs"
buildMap :: String -> Map.Map Int Line
buildMap s = do
  let withNumbers = filter (\(_, l) -> "#line" `isPrefixOf` l) $ zip [1 ..] $ lines s
  let directives = flip mapMaybe withNumbers $ \(n0, l) ->
        case words l of
          (_ : n1 : p) -> Just $ (,) (n0 :: Int) Line {num = read n1 :: Int, path = normalise $ drop 1 $ init $ unwords p}
          _ -> Nothing
  Map.fromList directives

resolve :: Map.Map Int Line -> Int -> Maybe Line
resolve m n = n `Map.lookupLE` m <&> \(n0, l@Line {num = n1}) -> l {num = n - n0 + n1 - 1}
