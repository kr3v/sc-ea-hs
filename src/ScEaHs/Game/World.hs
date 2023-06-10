{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module ScEaHs.Game.World where

import Control.Lens (makeLenses, (^.), At (at))
import qualified Data.Map.Strict as Map
import Graphics.Gloss (Point, Vector)
import Graphics.Gloss.Data.Color (Color)
import Graphics.Gloss.Data.Picture (Picture)
import Graphics.Gloss.Interface.IO.Game (SpecialKey)
import Numeric (showFFloat)
import ScEaHs.Game.Explosion (Explosion (..))
import ScEaHs.Game.Projectile (Projectile)
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator)
import ScEaHs.Utils.Format (showF2, showTF2)
import GHC.Generics (Generic)

data SStatus = WSS_PLAYER_INPUT | WSS_TURN_IN_PROGRESS deriving (Show, Eq)

data Status = Status
  { _turn :: Int,
    _wstatus :: SStatus
  }
  deriving (Show)

data Player = Player
  { _pos :: Point,
    _pcolor :: Color,
    _health :: Float
  }
  deriving (Show)

data World = World
  { _surfaceG :: SurfaceWithGenerator,
    _players :: Map.Map Int Player,
    _projectile :: Maybe Projectile,
    _explosion :: Maybe Explosion,
    _status :: Status,
    _score :: Map.Map Int Int
  }
  deriving (Generic)

$(makeLenses ''Player)
$(makeLenses ''World)
$(makeLenses ''Status)

player :: World -> Int -> Maybe Player
player w idx = w ^. players . at idx

currentPlayerTurn :: World -> Int
currentPlayerTurn w = w ^. status . turn

currentPlayer :: World -> Maybe Player
currentPlayer w = Map.lookup (currentPlayerTurn w) (w ^. players)
