{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module ScEaHs.Game.Projectile where

import Control.Lens (makeLenses)
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Vector (Vector)
import ScEaHs.Utils.Format (showTF2)

data ProjectileType = SHELL deriving (Show)

data Projectile = Projectile
  { _ppos :: Point,
    _speed :: Vector,
    _ptype :: ProjectileType
  }

instance Show Projectile where
  show :: Projectile -> String
  show (Projectile p s t) = "Projectile " ++ showTF2 p ++ " " ++ showTF2 s ++ " " ++ show t

$(makeLenses ''Projectile)
