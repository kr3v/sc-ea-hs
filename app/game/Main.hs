-- suggestion to copilot: I'm writing a scorched earth game
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens (over, view)
import Control.Monad.State (execState, runState)
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Gloss (Display (..), blue, red, translate, white)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (..), playIO)
import ScEaHs.GUI.Game (playerControlsModify, projectileLaunch, tick21, tick22)
import ScEaHs.GUI.Player.Controls (PlayerControls (..))
import ScEaHs.GUI.Render (Renderable (..))
import qualified ScEaHs.GUI.Render.Symbols as Symbols
import ScEaHs.GUI.World (PressedKeyState (..), ProjectileHistory (..), keysPressed, world)
import qualified ScEaHs.GUI.World as GUI
import ScEaHs.Game (tick1)
import ScEaHs.Game.Surface (putOn')
import ScEaHs.Game.Surface.Generator (generateSurface, surface, surfaceWithGenerator)
import ScEaHs.Game.World (SStatus (..))
import qualified ScEaHs.Game.World as Game
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))
import System.Random (mkStdGen)

---

specialKeyUpHandler :: SpecialKey -> GUI.World -> GUI.World
specialKeyUpHandler k = over keysPressed (Map.delete k) . playerControlsModify k

specialKeyDownHandler :: SpecialKey -> GUI.World -> GUI.World
specialKeyDownHandler k = over keysPressed (Map.insert k (PressedKeyState 0.0))

eventHandler :: Event -> GUI.World -> IO GUI.World
eventHandler _ w@(GUI.World {_world = Game.World {_status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}}) = return w
eventHandler (EventKey (SpecialKey KeySpace) Up _ _) w = return $ execState projectileLaunch w
eventHandler (EventKey (SpecialKey c) Up _ _) w = return $ specialKeyUpHandler c w
eventHandler (EventKey (SpecialKey c) Down _ _) w = return $ specialKeyDownHandler c w
eventHandler e w = print e >> return w

tick :: Float -> GUI.World -> GUI.World
tick df w = tick22 df $ over world (tick1 df) $ tick21 df w

-- todo: history - change to Map Int ...
--                 add angle/strength + source position
--                 compress to two lines
--       win/lose - beautify score

main :: IO ()
main = do
  w <- getPOSIXTime
  let g = mkStdGen $ round w

  let mx = 1000 :: Int
      my = 1000 :: Int
      windowSize = (mx, my)
      transformer = translate (-fromIntegral mx / 2) (-fromIntegral my / 2)
      sfg = surfaceWithGenerator g (runState $ generateSurface mx my)
      sf = view surface sfg

      player1 = Game.Player (putOn' sf (250, 0)) red 100
      player1Controls = PlayerControls (wrap 60) (wrap 50)
      player2 = Game.Player (putOn' sf (750, 0)) blue 100
      player2Controls = PlayerControls (wrap (180 - 60)) (wrap 50)
      players = Map.fromList [(1, player1), (2, player2)]

  let world_game :: Game.World = Game.World {_surfaceG = sfg, Game._players = players, _projectile = Nothing, _explosion = Nothing, _status = Game.Status 1 WSS_PLAYER_INPUT, _score = Map.empty}
      world :: GUI.World = GUI.World {_world = world_game, _playersControls = Map.fromList [(1, player1Controls), (2, player2Controls)], _projectileHistory = ProjectileHistory {_hits = [], _pictures = Symbols.pictures, _picturesIdx = 0}, _transformer = transformer, _keysPressed = Map.empty}

  print sf

  playIO
    (InWindow "GameEvent" windowSize (10, 10))
    white
    60
    world
    (return . render)
    eventHandler
    (\df w -> return $ tick df w)
