{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ScEaHs.GUI.Game where

import Control.Lens (Lens', Zoom (..), over, set, view, (%%=), (%=), (.=), (<<%=), (^.))
import Control.Monad.State (State, execState, gets)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Graphics.Gloss (color)
import Graphics.Gloss.Interface.IO.Game (SpecialKey (..))
import qualified ScEaHs.GUI.Player as GUI
import ScEaHs.GUI.Player.Controls (PlayerControls (..), angle, str)
import ScEaHs.GUI.World (PressedKeyState (..), ProjectileHit (..), World (..), currentPlayer, hits, keysPressed, pictures, picturesIdx, projectileHistory, world)
import qualified ScEaHs.GUI.World as GUI
import ScEaHs.Game (losers, nextPlayerMove)
import qualified ScEaHs.Game as Game
import ScEaHs.Game.Projectile (Projectile (..), ProjectileType (..))
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator (..))
import ScEaHs.Game.World (Explosion (..), SStatus (..), Status (..), World (..), epos, explosion, status, turn)
import qualified ScEaHs.Game.World as Game
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))
import ScEaHs.Utils.List (uncons')

---

playerControlsModify'' :: BoundedPlus b => Lens' PlayerControls b -> Int -> PlayerControls -> PlayerControls
playerControlsModify'' a d = over a (<+> d)

playerControlsModify' :: SpecialKey -> PlayerControls -> PlayerControls
playerControlsModify' KeyLeft = playerControlsModify'' angle 1
playerControlsModify' KeyRight = playerControlsModify'' angle (-1)
playerControlsModify' KeyDown = playerControlsModify'' str (-1)
playerControlsModify' KeyUp = playerControlsModify'' str 1
playerControlsModify' _ = id

playerControlsModify :: SpecialKey -> GUI.World -> GUI.World
playerControlsModify c w = over GUI.playersControls (Map.update (Just . playerControlsModify' c) (view (world . status . turn) w)) w

---

createProjectile :: GUI.Player -> Projectile
createProjectile (GUI.Player (Game.Player (x0, y0) c _) (PlayerControls a s)) =
  let s' :: Float = fromIntegral (unwrap s)
      a' :: Float = fromIntegral (unwrap a) * pi / 180
      vx = s' * cos a'
      vy = s' * sin a'
   in Projectile (x0, y0 + 5) (vx, vy) SHELL

projectileLaunch :: State GUI.World ()
projectileLaunch = do
  p <- gets $ fromJust . currentPlayer
  zoom world $ Game.projectileLaunch $ createProjectile p
  keysPressed .= Map.empty

explosionAddToHistory :: State GUI.World ()
explosionAddToHistory = do
  hs <- projectileHistory . pictures %%= uncons'
  idx <- projectileHistory . picturesIdx <<%= (+ 1)
  (GUI.Player (Game.Player _ pc _) c) <- gets $ fromJust . currentPlayer
  e <- gets $ fromJust . view (world . explosion)
  let ph = ProjectileHit (e ^. epos) c (color pc hs) idx
  projectileHistory . hits %= (ph :)

tick21 :: Float -> GUI.World -> GUI.World
tick21 df w@(GUI.World {_world = Game.World {_status = Game.Status {_wstatus = WSS_PLAYER_INPUT}}, _keysPressed = ks})
  | null ks = w
  | otherwise =
      let keyPressedTickHandler f k (PressedKeyState t) = (if t > 0.1 && (t + df) / 0.2 > t / 0.2 then playerControlsModify k . f else f, PressedKeyState (t + df))
          (c, ks') = Map.mapAccumRWithKey keyPressedTickHandler id ks
       in set keysPressed ks' . c $ w
tick21 df w@(GUI.World {_world = Game.World {_surfaceG = SurfaceWithGenerator {_surface = s}, _explosion = Just e@(Explosion c@(x, y) r mr), _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}})
  | r >= mr = execState explosionAddToHistory w
tick21 df w@(GUI.World {_world = w'@Game.World {_players = players, _explosion = Nothing, _projectile = Nothing, _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}}) =
  let ls = losers w'
   in if Map.null ls
        then over (world . status) (execState nextPlayerMove) w
        else set (projectileHistory . hits) [] w
tick21 df w = w

tick22 :: Float -> GUI.World -> GUI.World
tick22 df w = w
