{-# LANGUAGE TemplateHaskell #-}

module ScEaHs.GUI.Player where

import Control.Lens (makeLenses)
import ScEaHs.GUI.Player.Controls (PlayerControls)
import qualified ScEaHs.Game.World as Game

data Player = Player
  { _object :: Game.Player,
    _controls :: PlayerControls
  }
  deriving (Show)

$(makeLenses ''Player)