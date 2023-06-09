{-# LANGUAGE DeriveGeneric #-}
-- suggestion to copilot: I'm writing a scorched earth game
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Lens (makeLenses, over, view, (%=), (&), (.~), (^.))
import Control.Monad.State (MonadState (..), State, execState, modify, runState, evalState)
import Data.Data (Proxy (..))
import qualified Data.Map.Strict as Map
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Graphics.Gloss (Display (..), Picture (..), blue, red, translate, white)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (..), playIO)
import ScEaHs.GUI.Plugins.Controls (ControlsPlugin (..), PlayerControls (..))
import ScEaHs.GUI.Plugins.History (HistoryPlugin (..), ProjectileHistory (..), symbolsPictures)
import ScEaHs.GUI.Render (Renderable (..), RenderableS (..))
import ScEaHs.Game (tick1)
import ScEaHs.Game.Surface (putOn')
import ScEaHs.Game.Surface.Generator (generateSurface, surface, surfaceWithGenerator)
import ScEaHs.Game.World (SStatus (..))
import qualified ScEaHs.Game.World as Game
import ScEaHs.Plugin (eventP, tickP)
import qualified ScEaHs.Plugin as Plugin
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))
import System.Random (mkStdGen)

---

data World = World
  { _world :: Game.World,
    _history :: HistoryPlugin,
    _controls :: ControlsPlugin
  }
  deriving (Generic)

$(makeLenses ''World)

tick :: Float -> State World ()
tick df = do
  world %= tick1 df
  tickP (Proxy @HistoryPlugin) df
  tickP (Proxy @ControlsPlugin) df

event :: Event -> State World ()
event e = do
  eventP (Proxy @HistoryPlugin) e
  eventP (Proxy @ControlsPlugin) e

renderW :: World -> Picture
renderW w = Pictures [render $ w ^. world, render $ w ^. history, evalState (renderS (w ^. controls)) w]

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
      history = HistoryPlugin {_history = ProjectileHistory {_hits = [], _pictures = symbolsPictures, _picturesIdx = 0}}
      controls = ControlsPlugin {_playersControls = Map.fromList [(1, player1Controls), (2, player2Controls)], _keysPressed = Map.empty}
      world :: World = World {_world = world_game, _history = history, _controls = controls}

  print sf

  playIO
    (InWindow "GameEvent" windowSize (10, 10))
    white
    60
    world
    (return . transformer . renderW)
    (\e w -> return $ execState (event e) w)
    (\df w -> return $ execState (tick df) w)
