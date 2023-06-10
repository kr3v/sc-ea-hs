{-# LANGUAGE TupleSections #-}

module ScEaHs.Game.Explosion.Logic where

import Control.Lens (Bifunctor (..), Each (..), Zoom (..), (%=))
import Control.Lens.Combinators (over, to, use)
import Control.Monad.State (State, when)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import ScEaHs.Game.Explosion (Explosion (..))
import ScEaHs.Game.Surface (Surface, heights, isWithinSurface, putOn')
import ScEaHs.Game.World (Player, health, pos)
import ScEaHs.Utils.Geometry (distance, intersectionCircleVerticalLine)

explosionUpdateSurface :: Explosion -> Surface -> Surface
explosionUpdateSurface (Explosion e@(x, y) r mr _) s =
  let xs = filter (isWithinSurface s) [x - mr .. x + mr]
      hs = (\x -> snd $ putOn' s (x, 0)) <$> xs

      decide h x' ((x1, y1), (x2, y2))
        | y2 < h = h - (y2 - y1)
        | y1 < h = y1
        | otherwise = h
      decide' (h, x, i12) = (x, max (decide h x i12) 5)

      z = Map.fromList (bimap round round <$> (decide' <$> catMaybes (zipWith (\h x -> (h,x,) <$> intersectionCircleVerticalLine e mr x) hs xs)))
   in over heights (Map.union z) s

-- todo: consider higher d hp / consider extending explosion radius
explosionApplyHitIfNecessary :: Explosion -> State Player ()
explosionApplyHitIfNecessary (Explosion c _ mr _) = do
  d <- use $ pos . to (distance c)
  let hp_delta = (1 - d / mr) * 2 * 100
  when (d <= mr) $ health %= flip (-) hp_delta
