{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ScEaHs.Game.Projectile where

import Control.Lens (makeLenses, (.=))
import Control.Monad.State (State)
import Data.List (find, unfoldr)
import qualified Data.Map.Strict as Map
import Graphics.Gloss (Point)
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Vector (Vector)
import ScEaHs.Game.Surface (Surface, isWithinSurface, putOn')
import ScEaHs.Utils.Format (showF2, showTF2)

data ProjectileType = SHELL deriving (Show)

data Projectile = Projectile
  { _ppos :: Point,
    _speed :: Vector,
    _ptype :: ProjectileType,
    _psrc :: ProjectileSource
  }

instance Show Projectile where
  show :: Projectile -> String
  show (Projectile p s t _) = "Projectile " ++ showTF2 p ++ " " ++ showTF2 s ++ " " ++ show t

data ProjectileSource = ProjectileSource
  { _ifrom :: Point,
    _ivel :: Vector,
    _icontrols :: (Float, Float)
  }

$(makeLenses ''Projectile)
$(makeLenses ''ProjectileSource)

animationSpeed :: Float
animationSpeed = 10

gravityForce :: Float
gravityForce = 10

produce :: Float -> Float -> [Float]
produce x1 x2 = concat $ unfoldr (\x -> if x <= x2 then Just ([x - 0.01, x + 0.01], x + 1) else Nothing) (fromIntegral $ floor x1)

projectileTick :: Float -> Surface -> Projectile -> (Maybe Point, Maybe Projectile)
projectileTick df s (Projectile (x0, y0) (vx, vy) t src) =
  let dx = vx * animationSpeed * df
      dy = vy * animationSpeed * df
      x1 = x0 + dx
      y1 = y0 + dy
      p = Projectile (x1, y1) (vx, vy - gravityForce * animationSpeed * df) t src
      xs = filter (isWithinSurface s) $ produce (min x0 x1) (max x0 x1)
      ys = (\x -> if dx < 0.01 then y0 else (x - x0) / dx * dy + y0) <$> xs
      hs = (\x -> snd $ putOn' s (x, 0)) <$> xs
      collision = (\(x, _, h) -> (x, h)) <$> find (\(_, y, h) -> y < h) (zip3 xs ys hs)
   in (collision, if isWithinSurface s x1 then Just p else Nothing)