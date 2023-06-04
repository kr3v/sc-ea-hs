-- suggestion to copilot: I'm writing a scorched earth game
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens (At (at), Bifunctor (bimap), Each (each), Lens', Zoom (zoom), makeLenses, non, over, set, use, view, (%%=), (%%~), (%=), (%~), (&), (.=), (.~), (<%=), (<.=), (<<%=), (^.))
import Control.Lens.Lens (Lens', (&), (<<%~))
import Control.Lens.Prism (_Just)
import Control.Monad (liftM2)
import Control.Monad.State (MonadState (get, put), State, execState, gets, modify, runState, state)
import Data.Coerce (coerce)
import Data.Fixed ()
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.List (subsequences, uncons, unfoldr)
import Data.List.Index (imap)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, maybeToList)
import qualified Data.Set as Set
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Gloss.Data.Color (Color, blue, greyN, orange, red, white)
import Graphics.Gloss.Data.Display (Display (InWindow))
import Graphics.Gloss.Data.Picture (Picture (..), blank, circle, color, line, rectangleSolid, rotate, scale, text, translate)
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Vector (Vector)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), Modifiers, MouseButton (..), SpecialKey (..), black, circleSolid, playIO)
import Graphics.Gloss.Interface.Pure.Game (play)
import Numeric (showFFloat)
import ScEaHs.GUI.Player (controls, object)
import qualified ScEaHs.GUI.Player as GUI
import ScEaHs.GUI.Player.Controls (PlayerControls (..), angle, str)
import ScEaHs.GUI.World (PressedKeyState (..), ProjectileHit (..), currentPlayer, hits, keysPressed, pictures, picturesIdx, projectileHistory, world, ProjectileHistory (..))
import qualified ScEaHs.GUI.World as GUI
import ScEaHs.Game.Projectile (Projectile (..), ProjectileType (..))
import ScEaHs.Game.Surface (Surface (..), heights, isWithinSurface, putOn, putOn')
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator (SurfaceWithGenerator, _surface), generateSurface, surface, surfaceWithGenerator, updateSurface)
import ScEaHs.Game.World (Explosion (..), SStatus (..), epos, explosion, health, players, pos, projectile, score, status, surfaceG, turn, wstatus, _explosion, _status, radius)
import qualified ScEaHs.Game.World as Game
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))
import ScEaHs.Utils.Geometry (distance, intersectionCircleVerticalLine)
import System.Random (StdGen, mkStdGen)
import System.Random.Stateful (Random (randomR), StdGen, newStdGen)
import ScEaHs.GUI.Render (Renderable(..))

nextPlayerMove :: State Game.Status ()
nextPlayerMove = do
  t <- use turn
  turn .= if t == 1 then 2 else 1
  wstatus .= WSS_PLAYER_INPUT

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

specialKeyUpHandler :: SpecialKey -> GUI.World -> GUI.World
specialKeyUpHandler k = over keysPressed (Map.delete k) . playerControlsModify k

specialKeyDownHandler :: SpecialKey -> GUI.World -> GUI.World
specialKeyDownHandler k = over keysPressed (Map.insert k (PressedKeyState 0.0))

projectileLaunch' :: GUI.Player -> Projectile
projectileLaunch' (GUI.Player (Game.Player (x0, y0) c _) (PlayerControls a s)) =
  let s' :: Float = fromIntegral (unwrap s)
      a' :: Float = fromIntegral (unwrap a) * pi / 180
      vx = s' * cos a'
      vy = s' * sin a'
   in Projectile (x0, y0 + 5) (vx, vy) SHELL

projectileLaunch :: State GUI.World ()
projectileLaunch = do
  p <- gets $ fromJust . currentPlayer
  world . status . wstatus .= WSS_TURN_IN_PROGRESS
  world . projectile .= Just (projectileLaunch' p)
  keysPressed .= Map.empty

eventHandler :: Event -> GUI.World -> IO GUI.World
eventHandler _ w@(GUI.World {_world = Game.World {_status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}}) = return w
eventHandler (EventKey (SpecialKey KeySpace) Up _ _) w = return $ execState projectileLaunch w
eventHandler (EventKey (SpecialKey c) Up _ _) w = return $ specialKeyUpHandler c w
eventHandler (EventKey (SpecialKey c) Down _ _) w = return $ specialKeyDownHandler c w
eventHandler e w = print e >> return w

produce :: Float -> Float -> [Float]
produce x1 x2 = concat $ unfoldr (\x -> if x <= x2 then Just ([x - 0.01, x + 0.01], x + 1) else Nothing) (fromIntegral $ floor x1)

animationSpeed :: Float
animationSpeed = 10

projectileTick :: Float -> Surface -> Projectile -> (Maybe Point, Maybe Projectile)
projectileTick f s (Projectile (x0, y0) (vx, vy) t) =
  let dx = vx * animationSpeed * f
      dy = vy * animationSpeed * f
      x1 = x0 + dx
      y1 = y0 + dy
      p = Projectile (x1, y1) (vx, vy - 10 * animationSpeed * f) t
      xs = filter (isWithinSurface s) $ produce (min x0 x1) (max x0 x1)
      ys = (\x -> if dx < 0.01 then y0 else (x - x0) / dx * dy + y0) <$> xs
      hs = (\x -> snd $ putOn' s (x, 0)) <$> xs
      collision = (\(x, y, _) -> (x, y)) <$> find (\(_, y, h) -> y < h) (zip3 xs ys hs)
   in (collision, if isWithinSurface s x1 then Just p else Nothing)

explosionUpdateSurface :: Explosion -> Surface -> Surface
explosionUpdateSurface (Explosion e@(x, y) r mr) s =
  let xs = filter (isWithinSurface s) [x - mr .. x + mr]
      hs = (\x -> snd $ putOn' s (x, 0)) <$> xs

      decide h x' ((x1, y1), (x2, y2))
        | y2 < h = h - (y2 - y1)
        | y1 < h = y1
        | otherwise = h
      decide' (h, x, i12) = (x, max (decide h x i12) 5)

      z = Map.fromList (bimap round round <$> (decide' <$> catMaybes (zipWith (\h x -> (h,x,) <$> intersectionCircleVerticalLine e mr x) hs xs)))
   in over heights (Map.union z) s

uncons' :: [a] -> (a, [a])
uncons' = fromJust . uncons

explosionAddToHistory :: GUI.World -> GUI.World
explosionAddToHistory = execState explosionAddToHistory'

explosionAddToHistory' :: State GUI.World ()
explosionAddToHistory' = do
  hs <- projectileHistory . pictures %%= uncons'
  idx <- projectileHistory . picturesIdx <<%= (+ 1)
  (GUI.Player (Game.Player _ pc _) c) <- gets $ fromJust . currentPlayer
  e <- gets $ fromJust . view (world . explosion)
  let ph = ProjectileHit (e ^. epos) c (color pc hs) idx
  projectileHistory . hits %= (ph :)

-- todo: consider higher d hp / consider extending explosion radius
explosionCheckPlayerHit' :: Explosion -> Game.Player -> Game.Player
explosionCheckPlayerHit' (Explosion c _ mr) p =
  let d = distance c $ view pos p
      hp_delta = if d > mr then 0 else (1 - d / mr) * 2 * 100
   in over health (flip (-) hp_delta) p

explosionCheckPlayerHit :: Game.World -> Game.World
explosionCheckPlayerHit w = over (players . each) (explosionCheckPlayerHit' (fromJust $ view explosion w)) w

-- todo: consider removing hp for falling down
putPlayersOnSurface :: State Game.World ()
putPlayersOnSurface = do
  s <- use $ surfaceG . surface
  players . each . pos %= putOn' s

weveGotAWinner :: Map.Map Int GUI.Player -> State GUI.World ()
weveGotAWinner losers = do
  zoom (world . surfaceG) updateSurface
  zoom (world . status) nextPlayerMove
  zoom world putPlayersOnSurface
  world . players . each . health .= 100
  projectileHistory . hits .= []
  let z = -1 <$ losers
  world . score %= Map.unionWith (+) z

-- tick :: Float -> Game.World -> Game.World


tick :: Float -> GUI.World -> IO GUI.World
tick df w@(GUI.World {_world = Game.World {_status = Game.Status {_wstatus = WSS_PLAYER_INPUT}}, _keysPressed = ks})
  | null ks = return w
  | otherwise =
      let keyPressedTickHandler f k (PressedKeyState t) = (if t > 0.1 && (t + df) / 0.2 > t / 0.2 then playerControlsModify k . f else f, PressedKeyState (t + df))
          (c, ks') = Map.mapAccumRWithKey keyPressedTickHandler id ks
       in return $ set keysPressed ks' . c $ w
tick df w@(GUI.World {_world = Game.World {_surfaceG = SurfaceWithGenerator {_surface = s}, _projectile = (Just p), _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}}) =
  let (c, p') = projectileTick df s p
   in return $ case c of
        Just c' -> set (world . projectile) Nothing . set (world . explosion) (Just $ Explosion c' 0 10) $ w
        Nothing -> set (world . projectile) p' w
tick df w@(GUI.World {_world = Game.World {_surfaceG = SurfaceWithGenerator {_surface = s}, _explosion = Just e@(Explosion c@(x, y) r mr), _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}})
  | r < mr = return $ over (world . explosion . _Just . radius) ((df * animationSpeed) +) w
  | otherwise = return $ set (world . explosion) Nothing . execState (zoom world putPlayersOnSurface) . over (world . surfaceG . surface) (explosionUpdateSurface e) . over world explosionCheckPlayerHit . explosionAddToHistory $ w
tick df w@(GUI.World {}) = do
  let ps = GUI.players w
      ps' = Map.filter ((<= 0) . view (object . health)) ps
   in if Map.null ps'
        then return $ over (world . status) (execState nextPlayerMove) w
        else return $ execState (weveGotAWinner ps') w

---

historySymbols :: [Picture]
historySymbols =
  [ translate (-0.4) (-0.4) $ circle 4,
    line [(-5, -5), (5, 5)] <> line [(-5, 5), (5, -5)], -- x
    line [(-5, 0), (5, 0)] <> line [(0, -5), (0, 5)], -- +
    line [(-5, -5), (-5, 5)] <> line [(5, -5), (5, 5)] <> line [(-5, -5), (5, -5)] <> line [(-5, 5), (5, 5)], -- square
    line [(-5, 0), (0, 5)] <> line [(0, 5), (5, 0)] <> line [(5, 0), (0, -5)] <> line [(0, -5), (-5, 0)], -- diamond
    line [(-5, -5), (5, -5)] <> line [(5, -5), (0, 5)] <> line [(0, 5), (-5, -5)] -- triangle
  ]

historyPictures :: [Picture]
historyPictures = Pictures <$> filter (liftM2 (&&) (> 0) (<= 2) . length) (subsequences historySymbols)

historyPictures' :: [Picture]
historyPictures' = cycle historyPictures

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

      world_game :: Game.World = Game.World {_surfaceG = sfg, Game._players = players, _projectile= Nothing, _explosion = Nothing, _status = Game.Status 1 WSS_PLAYER_INPUT, _score = Map.empty}
      world :: GUI.World = GUI.World {_world = world_game, _playersControls = Map.fromList [(1, player1Controls), (2, player2Controls)], _projectileHistory = ProjectileHistory{_hits = [], _pictures = historyPictures', _picturesIdx = 0}, _transformer = transformer, _keysPressed = Map.empty}

  print $ length historyPictures
  print sf

  playIO
    (InWindow "GameEvent" windowSize (10, 10))
    white
    60
    world
    (return . render)
    eventHandler
    tick
