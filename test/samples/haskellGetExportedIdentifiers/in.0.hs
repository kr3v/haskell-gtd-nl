{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module ScEaHs.Game.Surface (Surface (..), module ScEaHs.Game.Surface.Generator, putOn, putOn', isWithinSurface, maxHeight, width, heights) where

import Control.Lens (makeLenses)
import qualified Data.Map.Strict as Map
import Graphics.Gloss (Point)
import System.Random.Stateful (StdGen)

data Surface = Surface
  { _maxHeight :: Int,
    _width :: Int,
    _heights :: Map.Map Int Int
  }
  deriving (Show)

$(makeLenses ''Surface)

instance Renderable Surface where
  render :: Surface -> Picture
  render (Surface _ _ hs) = Pictures $ imap (\x h -> translate (fromIntegral x) (fromIntegral (h `div` 2)) $ rectangleSolid 1 (fromIntegral h)) $ Map.elems hs

putOn :: Surface -> Point -> Maybe Point
putOn (Surface mh mw hs) (x, y) = (x,) . fromIntegral <$> Map.lookup (round x) hs

putOn' :: Surface -> Point -> Point
putOn' (Surface mh mw hs) (x, y) = (x,) . fromIntegral $ hs Map.! round x

isWithinSurface :: Surface -> Float -> Bool
isWithinSurface (Surface mh mw hs) x = x >= 0 && x <= fromIntegral mw
