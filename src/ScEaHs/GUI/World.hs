{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ScEaHs.GUI.World where

import Control.Lens (At (at), makeLenses, view, (^.))
import qualified Data.Map.Strict as Map
import Graphics.Gloss.Data.Picture (Picture, Point)
import Graphics.Gloss.Interface.IO.Game (SpecialKey)
import ScEaHs.GUI.Player (Player)
import qualified ScEaHs.GUI.Player as GUI
import ScEaHs.GUI.Player.Controls (PlayerControls)
import qualified ScEaHs.Game.World as Game
import Data.Maybe (catMaybes)

newtype PressedKeyState = PressedKeyState
  { _time :: Float
  }
  deriving (Show)

data ProjectileHit = ProjectileHit
  { _hpos :: Point,
    _cs :: PlayerControls,
    _pic :: Picture,
    _idx :: Int
  }

data ProjectileHistory = ProjectileHistory
  { _hits :: [ProjectileHit],
    _pictures :: [Picture],
    _picturesIdx :: Int
  }

data World = World
  { _world :: Game.World,
    _transformer :: Picture -> Picture,
    _playersControls :: Map.Map Int PlayerControls,
    _keysPressed :: Map.Map SpecialKey PressedKeyState,
    _projectileHistory :: ProjectileHistory
  }

$(makeLenses ''PressedKeyState)
$(makeLenses ''ProjectileHit)
$(makeLenses ''ProjectileHistory)
$(makeLenses ''World)

players :: World -> Map.Map Int GUI.Player
players w = Map.fromList $ catMaybes [(1,) <$> player w 1, (2,) <$> player w 2]

player :: World -> Int -> Maybe GUI.Player
player w idx =
  let p = w ^. world . Game.players . at idx
      c = w ^. playersControls . at idx
   in GUI.Player <$> p <*> c

player' :: World -> (Int, Game.Player) -> Maybe GUI.Player
player' w (idx, p) =
  let c = w ^. playersControls . at idx
   in GUI.Player p <$> c

currentPlayer :: World -> Maybe GUI.Player
currentPlayer w = player w $ currentPlayerTurn w

currentPlayerTurn :: World -> Int
currentPlayerTurn w = Game.currentPlayerTurn $ view world w
