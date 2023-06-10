{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ScEaHs.GUI.Plugins.Controls where

import Control.Lens (At (..), Ixed (..), Lens', Zoom (zoom), makeLenses, use, (%=), (.=))
import Control.Lens.Prism (_Just)
import Control.Monad.State (State, execState, gets)
import Data.Generics.Product (HasType (..))
import qualified Data.Generics.Product
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (SpecialKey), KeyState (Down, Up), SpecialKey (KeyDown, KeyLeft, KeyRight, KeySpace, KeyUp))
import qualified ScEaHs.Game as Game
import ScEaHs.Game.Projectile (Projectile (..), ProjectileSource (..), ProjectileType (..))
import ScEaHs.Game.World (SStatus (..), Status (..), World (..))
import qualified ScEaHs.Game.World as Game
import ScEaHs.Plugin (GamePlugin (..), GamePluginState)
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

---

newtype PressedKeyState = PressedKeyState
  { _time :: Float
  }
  deriving (Show)

data ControlsPlugin = ControlsPlugin
  { _playersControls :: Map.Map Int PlayerControls,
    _keysPressed :: Map.Map SpecialKey PressedKeyState
  }
  deriving (Show)

type ControlsPluginState s = GamePluginState ControlsPlugin s

$(makeLenses ''ControlsPlugin)
$(makeLenses ''PressedKeyState)

playerControlsModify'' :: BoundedPlus b => Lens' PlayerControls b -> Int -> State PlayerControls ()
playerControlsModify'' a d = a %= (<+> d)

playerControlsModify' :: SpecialKey -> State PlayerControls ()
playerControlsModify' KeyLeft = playerControlsModify'' angle 1
playerControlsModify' KeyRight = playerControlsModify'' angle (-1)
playerControlsModify' KeyDown = playerControlsModify'' str (-1)
playerControlsModify' KeyUp = playerControlsModify'' str 1
playerControlsModify' _ = return ()

playerControlsModify :: SpecialKey -> ControlsPluginState s
playerControlsModify k = do
  w <- use $ typed @World
  let p = Game.currentPlayerTurn w
  zoom (typed @ControlsPlugin . playersControls . ix p) (playerControlsModify' k)

specialKeyUpHandler :: SpecialKey -> ControlsPluginState s
specialKeyUpHandler k = do
  typed @ControlsPlugin . keysPressed %= Map.delete k
  playerControlsModify k

specialKeyDownHandler :: SpecialKey -> ControlsPluginState s
specialKeyDownHandler k = do
  typed @ControlsPlugin . keysPressed %= Map.insert k (PressedKeyState 0.0)

createProjectile :: PlayerControls -> Game.Player -> Projectile
createProjectile (PlayerControls a s) (Game.Player (x0, y0) c _) =
  let s' :: Float = fromIntegral (unwrap s)
      a' :: Float = fromIntegral (unwrap a)
      aR :: Float = a' * pi / 180
      vx = s' * cos aR
      vy = s' * sin aR
   in Projectile (x0, y0 + 5) (vx, vy) SHELL ProjectileSource {_ifrom = (x0, y0), _ivel = (vx, vy), _icontrols = (a', s')}

projectileLaunch :: ControlsPluginState s
projectileLaunch = do
  w <- use $ typed @Game.World
  let p = fromJust $ Game.currentPlayer w
  c <- use $ typed @ControlsPlugin . playersControls . at (Game.currentPlayerTurn w)
  zoom (typed @Game.World) $ Game.projectileLaunch $ createProjectile (fromJust c) p
  (typed @ControlsPlugin . keysPressed) .= Map.empty

instance GamePlugin ControlsPlugin where
  event :: World -> ControlsPlugin -> Event -> ControlsPluginState s
  event Game.World {_status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}} a e = do return ()
  event w s (EventKey (SpecialKey KeySpace) Up _ _) = projectileLaunch
  event w s (EventKey (SpecialKey c) Up _ _) = specialKeyUpHandler c
  event w s (EventKey (SpecialKey c) Down _ _) = specialKeyDownHandler c
  event _ _ _ = return ()

  tick :: World -> ControlsPlugin -> Float -> ControlsPluginState s
  tick w@Game.World {_status = Game.Status {_wstatus = WSS_PLAYER_INPUT}} ControlsPlugin {_keysPressed = ks} df | not $ null ks = do
    let accum m k (PressedKeyState t) = (,) (if t > 0.1 then playerControlsModify k >> m else m) (PressedKeyState (t + df))
        (m, ks') = Map.mapAccumWithKey accum (return ()) ks
    (typed @ControlsPlugin . keysPressed) .= ks'
    m
  tick _ _ _ = return ()
