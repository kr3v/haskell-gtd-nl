{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ScEaHs.Game.Surface (Surface (..), module ScEaHs.Game.Surface.Generator, putOn) where

import Control.Lens (makeLenses)
import Data.Map.Lazy as Map
import qualified Data.Map.Strict as Map
import Graphics.Gloss (Picture (..), Point)
import ScEaHs.Game.Surface.Generator hiding (maxHeight, width, heights)

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