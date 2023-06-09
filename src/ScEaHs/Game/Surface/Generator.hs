{-# LANGUAGE TemplateHaskell #-}

module ScEaHs.Game.Surface.Generator (SurfaceWithGenerator (..), surfaceWithGenerator, mapGenerator, surface, updateSurface, generateSurface) where

import Control.Lens (makeLenses, use, (%%=), (.=))
import qualified Data.Map.Strict as Map
import ScEaHs.Game.Surface (Surface (..))
import System.Random.Stateful (StdGen, Random (randomR))
import Control.Monad.State (State, MonadState (state))
import qualified Data.Set as Set

data SurfaceWithGenerator = SurfaceWithGenerator
  { _surface :: Surface,
    _surfaceStdGen :: StdGen,
    _surfaceState :: StdGen -> (Surface, StdGen)
  }

surfaceWithGenerator :: StdGen -> (StdGen -> (Surface, StdGen)) -> SurfaceWithGenerator
surfaceWithGenerator stdGen state = do
  let (surface, stdGen') = state stdGen
  SurfaceWithGenerator surface stdGen' state

$(makeLenses ''SurfaceWithGenerator)

mapGenerator :: Int -> Float -> Int -> Int -> Int -> Int
mapGenerator offset seed mx my x =
  let x' = fromIntegral (x + offset) :: Float
      mx' = fromIntegral mx :: Float
      my' = fromIntegral my :: Float
      x'_norm = x' / mx'
      x'_norm_2pi = x'_norm * 2 * pi
      h_var0 = sin x'_norm_2pi * (my' / (4 * seed))
      h_var1 = cos (x'_norm_2pi * 2) * (my' / (8 * seed))
      h_var2 = sin (x'_norm_2pi * 4) * (my' / (16 * seed))
      h_var3 = cos (x'_norm_2pi * 8) * (my' / (32 * seed))
      h_var4 = sin (x'_norm_2pi * 16) * (my' / (64 * seed))
      h = round (h_var0 + h_var1 + h_var2 + h_var3 + h_var4 + my' / 3)
   in min (4 * my `div` 5) $ max h 5

updateSurface :: State SurfaceWithGenerator ()
updateSurface = do
  s <- use surfaceState
  s' <- surfaceStdGen %%= s
  surface .= s'

generateSurface :: Int -> Int -> State StdGen Surface
generateSurface mx my = do
  offset <- state $ randomR (0, 16384)
  seed <- state $ randomR (0.5, 10)
  let g = mapGenerator offset seed mx my
  return $ Surface my mx (Map.fromSet g (Set.fromList $ take (mx + 1) [0 ..]))