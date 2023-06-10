{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module ScEaHs.Game.Projectile where

import Control.Lens (makeLenses)
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Vector (Vector)
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

data Explosion = Explosion
  { _epos :: Point,
    _radius :: Float,
    _maxRadius :: Float,
    _src :: ProjectileSource
  }

instance Show Explosion where
  show :: Explosion -> String
  show (Explosion p r mr _) = "Explosion " ++ showTF2 p ++ " " ++ showF2 r ++ " " ++ showF2 mr

$(makeLenses ''Projectile)
$(makeLenses ''Explosion)
$(makeLenses ''ProjectileSource)
