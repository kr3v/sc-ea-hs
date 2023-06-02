{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ScEaHs.Game.Surface (Surface (..), putOn, putOn', isWithinSurface, maxHeight, width, heights) where

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

putOn :: Surface -> Point -> Maybe Point
putOn (Surface mh mw hs) (x, y) = (x,) . fromIntegral <$> Map.lookup (round x) hs

putOn' :: Surface -> Point -> Point
putOn' (Surface mh mw hs) (x, y) = (x,) . fromIntegral $ hs Map.! round x

isWithinSurface :: Surface -> Float -> Bool
isWithinSurface (Surface mh mw hs) x = x >= 0 && x <= fromIntegral mw
