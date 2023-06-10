{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module ScEaHs.Game.Explosion where

import Graphics.Gloss (Point)
import ScEaHs.Game.Projectile (ProjectileSource)
import Control.Lens (makeLenses)
import ScEaHs.Utils.Format (showF2, showTF2)

data Explosion = Explosion
  { _epos :: Point,
    _radius :: Float,
    _maxRadius :: Float,
    _src :: ProjectileSource
  }

instance Show Explosion where
  show :: Explosion -> String
  show (Explosion p r mr _) = "Explosion " ++ showTF2 p ++ " " ++ showF2 r ++ " " ++ showF2 mr

$(makeLenses ''Explosion)