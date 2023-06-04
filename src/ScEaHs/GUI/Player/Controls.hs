{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module ScEaHs.GUI.Player.Controls where

import Control.Lens (makeLenses)
import Graphics.Gloss.Interface.IO.Game (SpecialKey (..))
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))

newtype Angle = Angle Int deriving (Show)

newtype Strength = Strength Int deriving (Show)

instance BoundedPlus Angle where
  bound _ = 360

instance BoundedPlus Strength where
  bound _ = 200

data PlayerControls = PlayerControls
  { _angle :: Angle,
    _str :: Strength
  }
  deriving (Show)

$(makeLenses ''PlayerControls)
