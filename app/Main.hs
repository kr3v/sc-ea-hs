-- suggestion to copilot: I'm writing a scorched earth game
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens (Bifunctor (bimap), Each (each), Lens', makeLenses, over, set, view, (%~), (&), (.~), (^.), At (at), non, )
import Control.Lens.Prism (_Just)
import Control.Monad (liftM2)
import Data.Fixed ()
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (subsequences, unfoldr)
import Data.List.Index (imap)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, maybeToList)
import qualified Data.Set as Set
import GHC.RTS.Flags (TraceFlags (traceScheduler))
import Graphics.Gloss.Data.Color (Color, blue, greyN, orange, red, white)
import Graphics.Gloss.Data.Display (Display (InWindow))
import Graphics.Gloss.Data.Picture (Picture (..), blank, circle, color, line, rectangleSolid, rotate, scale, text, translate)
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Vector (Vector)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), Modifiers, MouseButton (..), SpecialKey (..), black, circleSolid, playIO)
import Graphics.Gloss.Interface.Pure.Game (play)
import Numeric (showFFloat)
import Control.Lens.Lens

---

newtype Angle = Angle Int deriving (Show)

newtype Strength = Strength Int deriving (Show)

class BoundedPlus a where
  bound :: a -> Int
  unwrap :: a -> Int
  wrap :: Int -> a

  (<+>) :: a -> Int -> a
  (<+>) a b = wrap $ (unwrap a + b) `mod` bound a

instance BoundedPlus Angle where
  bound :: Angle -> Int
  bound _ = 360
  unwrap :: Angle -> Int
  unwrap (Angle a) = a
  wrap :: Int -> Angle
  wrap = Angle

instance BoundedPlus Strength where
  bound :: Strength -> Int
  bound _ = 200
  unwrap :: Strength -> Int
  unwrap (Strength a) = a
  wrap :: Int -> Strength
  wrap = Strength

---

class Renderable a where
  render :: a -> Picture

class TextualInfo a where
  info :: a -> String

--

data WorldStatusState = WSS_PLAYER_INPUT | WSS_TURN_IN_PROGRESS deriving (Show, Eq)

data WorldStatus = WorldStatus
  { _turn :: Int,
    _state :: WorldStatusState
  }
  deriving (Show)

newtype PressedKeyState = PressedKeyState
  { _time :: Float
  }
  deriving (Show)

data World = World
  { _surface :: Surface,
    _players :: Map.Map Int Player,
    _projectile :: Maybe Projectile,
    _explosion :: Maybe Explosion,
    _projectilesHistory :: [ProjectileHit],
    _status :: WorldStatus,
    _keysPressed :: Map.Map SpecialKey PressedKeyState,
    _transformer :: Picture -> Picture,
    _projectileHitPictures :: [Picture]
  }

data Surface = Surface
  { _maxHeight :: Int,
    _width :: Int,
    _heights :: Map.Map Int Int
  }
  deriving (Show)

data PlayerControls = PlayerControls
  { _angle :: Angle,
    _str :: Strength
  }
  deriving (Show)

data PlayerObject = PlayerObject
  { _pos :: Point,
    _pcolor :: Color
  }
  deriving (Show)

data Player = Player
  { _object :: PlayerObject,
    _controls :: PlayerControls,
    _health :: Float
  }
  deriving (Show)

data ProjectileType = SHELL deriving (Show)

data Projectile = Projectile
  { _ppos :: Point,
    _speed :: Vector,
    _ptype :: ProjectileType
  }

data Explosion = Explosion
  { _epos :: Point,
    _radius :: Float,
    _maxRadius :: Float
  }

data ProjectileHit = ProjectileHit
  { _hpos :: Point,
    _desc :: String,
    _pic :: Picture
  }

showF2 :: RealFloat a => a -> String
showF2 = flip (showFFloat (Just 2)) ""

showTF2 :: RealFloat a => (a, a) -> String
showTF2 (a, b) = "(" ++ showF2 a ++ "," ++ showF2 b ++ ")"

instance Show Projectile where
  show :: Projectile -> String
  show (Projectile p s t) = "Projectile " ++ showTF2 p ++ " " ++ showTF2 s ++ " " ++ show t

instance Show Explosion where
  show :: Explosion -> String
  show (Explosion p r mr) = "Explosion " ++ showTF2 p ++ " " ++ showF2 r ++ " " ++ showF2 mr

$(makeLenses ''PressedKeyState)
$(makeLenses ''World)
$(makeLenses ''WorldStatus)
$(makeLenses ''Surface)
$(makeLenses ''PlayerControls)
$(makeLenses ''PlayerObject)
$(makeLenses ''Player)
$(makeLenses ''Projectile)
$(makeLenses ''Explosion)
$(makeLenses ''ProjectileHit)

---

instance Renderable World where
  render :: World -> Picture
  render w@(World s ps p ea ph st@(WorldStatus p' s') _ t _) =
    let surface = render s
        projectile = render <$> maybeToList p
        playersPictures = render <$> ps
        playersStatus = info <$> ps
        status = info st
        text' x y s = translate x y $ scale 0.2 0.2 $ text s
        explosion' = maybeToList $ render <$> ea
        playersStatus' = (\(i, p) -> text' 5 (1000 - 25 - 30 * fromIntegral i) $ "  player " ++ show i ++ ": " ++ info p) <$> Map.assocs ps
        projectileHitsDebugInfo = imap (\i ph -> text' (5 - 300) (1000 - 30 * fromIntegral i) ("hit #" ++ show i ++ ": " ++ info ph) <> translate (-300-10) (1000 + 7 - 30 * fromIntegral i) (ph ^. pic)) ph
        projectileHits = render <$> ph
        status' = text' 5 (1000 - 25) status
        debugInfo = text' 5 (1000 - 25 - 30 * 3) $ "explosion: " ++ show ea ++ ", projectile: " ++ show p
     in t $ Pictures $ surface : debugInfo : status' : Map.elems playersPictures ++ playersStatus' ++ projectile ++ explosion' ++ projectileHitsDebugInfo ++ projectileHits

instance TextualInfo WorldStatus where
  info :: WorldStatus -> String
  info (WorldStatus p s) = "player: " ++ show p ++ ", state: " ++ show s ++ (if s == WSS_PLAYER_INPUT then " (press space to make a turn)" else "")

instance Renderable Surface where
  render :: Surface -> Picture
  render (Surface _ _ hs) = Pictures $ imap (\x h -> translate (fromIntegral x) (fromIntegral (h `div` 2)) $ rectangleSolid 1 (fromIntegral h)) $ Map.elems hs

instance TextualInfo PlayerControls where
  info :: PlayerControls -> String
  info (PlayerControls a s) = "angle: " ++ show a ++ ", strength: " ++ show s

instance Renderable PlayerObject where
  render :: PlayerObject -> Picture
  render (PlayerObject (x, y) c) = color c $ translate x y $ rectangleSolid 10 10

instance Renderable Player where
  render :: Player -> Picture
  render (Player o@(PlayerObject (x, y) _) (PlayerControls a s) _) =
    let z x y = color (greyN 0.5) $ translate x y $ circleSolid 2
        a' = fromIntegral (unwrap a) * pi / 180
        step = 10
        ps = foldr (<>) (render o) $ (\i -> z (x + i * step * cos a') (y + i * step * sin a')) . fromIntegral <$> [1 .. (unwrap s `div` round step)]
     in ps

instance TextualInfo Player where
  info :: Player -> String
  info (Player o c h) = info c ++ ", health: " ++ showF2 h

instance Renderable Projectile where
  render :: Projectile -> Picture
  render (Projectile (x, y) _ _) = color black $ translate x y $ rectangleSolid 5 5

instance Renderable Explosion where
  render :: Explosion -> Picture
  render (Explosion (x, y) r mr) = color orange $ translate x y $ circleSolid r

instance TextualInfo ProjectileHit where
  info :: ProjectileHit -> String
  info (ProjectileHit p d _) = showTF2 p

instance Renderable ProjectileHit where
  render :: ProjectileHit -> Picture
  render (ProjectileHit p _ pic) = uncurry translate p pic

---

putOn :: Surface -> Point -> Maybe Point
putOn (Surface mh mw hs) (x, y) = (x,) . fromIntegral <$> Map.lookup (round x) hs

putOn' :: Surface -> Point -> Point
putOn' (Surface mh mw hs) (x, y) = (x,) . fromIntegral $ hs Map.! round x

intersectionCircleVerticalLine ::
  Point -> -- center of the circle
  Float -> -- radius of the circle
  Float -> -- x coordinate of vertical line
  Maybe (Point, Point) -- intersection points
intersectionCircleVerticalLine (x0, y0) r x
  | discriminant < 0 = Nothing
  | otherwise = Just ((x, y1), (x, y2))
  where
    discriminant = r ^ 2 - (x - x0) ^ 2
    y1 = y0 - sqrt discriminant
    y2 = y0 + sqrt discriminant

distance :: Point -> Point -> Float
distance (x0, y0) (x, y) = sqrt $ (x - x0) ^ 2 + (y - y0) ^ 2

sinSurface :: Int -> Int -> Int -> Int
sinSurface mx my x =
  let x' = fromIntegral x :: Float
      mx' = fromIntegral mx :: Float
      my' = fromIntegral my :: Float
      x'_norm = x' / mx'
      x'_norm_2pi = x'_norm * 2 * pi
      h_var0 = sin x'_norm_2pi * (my' / 40)
      h_var1 = sin (x'_norm_2pi * 2) * (my' / 80)
      h_var2 = sin (x'_norm_2pi * 4) * (my' / 160)
      h_var3 = sin (x'_norm_2pi * 8) * (my' / 320)
      h_var4 = sin (x'_norm_2pi * 16) * (my' / 640)
      h = round (h_var0 + h_var1 + h_var2 + h_var3 + h_var4 + my' / 3)
   in h

isWithinSurface :: Surface -> Float -> Bool
isWithinSurface (Surface mh mw hs) x = x >= 0 && x <= fromIntegral mw

---

nextPlayerMove :: World -> World
nextPlayerMove w@(World _ _ _ _ _ (WorldStatus t _) _ _ _) = set status (WorldStatus (if t == 1 then 2 else 1) WSS_PLAYER_INPUT) w

currentPlayer :: World -> Player
currentPlayer w = (w ^. players) Map.! (w ^. status . turn)

---

modify :: BoundedPlus b => Lens' PlayerControls b -> Int -> Player -> Player
modify a d = over (controls . a) (<+> d)

playerControlsModify' :: SpecialKey -> Player -> Player
playerControlsModify' KeyLeft = modify angle 1
playerControlsModify' KeyRight = modify angle (-1)
playerControlsModify' KeyDown = modify str (-1)
playerControlsModify' KeyUp = modify str 1
playerControlsModify' _ = id

playerControlsModify :: SpecialKey -> World -> World
playerControlsModify c w = over players (Map.update (Just . playerControlsModify' c) (view (status . turn) w)) w

specialKeyUpHandler :: SpecialKey -> World -> World
specialKeyUpHandler k = over keysPressed (Map.delete k) . playerControlsModify k

specialKeyDownHandler :: SpecialKey -> World -> World
specialKeyDownHandler k = over keysPressed (Map.insert k (PressedKeyState 0.0))

projectileLaunch' :: Player -> Projectile
projectileLaunch' (Player (PlayerObject (x0, y0) c) (PlayerControls a s) _) =
  let s' :: Float = fromIntegral (unwrap s)
      a' :: Float = fromIntegral (unwrap a) * pi / 180
      vx = s' * cos a'
      vy = s' * sin a'
   in Projectile (x0, y0 + 5) (vx, vy) SHELL

projectileLaunch :: World -> World
projectileLaunch w@(World _ ps _ _ _ st@(WorldStatus p _') _ _ _) = set (status . state) WSS_TURN_IN_PROGRESS . set projectile (Just $ projectileLaunch' $ ps Map.! p) . set keysPressed Map.empty $ w

eventHandler :: Event -> World -> IO World
eventHandler _ w@(World _ _ _ _ _ (WorldStatus _ WSS_TURN_IN_PROGRESS) _ _ _) = return w
eventHandler (EventKey (SpecialKey KeySpace) Up _ _) w = return $ projectileLaunch w
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

explosionUpdateSurface :: Surface -> Explosion -> Surface
explosionUpdateSurface s (Explosion e@(x, y) r mr) =
  let xs = filter (isWithinSurface s) [x - mr .. x + mr]
      hs = (\x -> snd $ putOn' s (x, 0)) <$> xs

      decide h x' ((x1, y1), (x2, y2))
        | y2 < h = h - (y2 - y1)
        | y1 < h = y1
        | otherwise = h
      decide' (h, x, i12) = (x, max (decide h x i12) 5)

      z = Map.fromList (bimap round round <$> (decide' <$> catMaybes (zipWith (\h x -> (h,x,) <$> intersectionCircleVerticalLine e mr x) hs xs)))
   in over heights (Map.union z) s

explosionAddToHistory :: World -> World
explosionAddToHistory w@(World _ _ _ (Just e@(Explosion c@(x, y) r mr)) ph _ _ _ _) = 
  let (p, w') = w & projectileHitPictures <<%~ tail
  in over projectilesHistory ((:) $ ProjectileHit c "" (color (currentPlayer w ^. object . pcolor) (head p))) w'

-- todo: consider higher d hp / consider extending explosion radius
explosionCheckPlayerHit' :: World -> Player -> Player
explosionCheckPlayerHit' (World _ _ _ (Just (Explosion c _ mr)) _ _ _ _ _) p =
  let d = distance c (view (object . pos) p)
      hp_delta = if d > mr then 0 else (1 - d / mr) * 2 * 100
   in over health (flip (-) hp_delta) p

explosionCheckPlayerHit :: World -> World
explosionCheckPlayerHit w = over (players . each) (explosionCheckPlayerHit' w) w

-- todo: consider removing hp for falling down
explosionUpdateSurfacePlayersUpdate :: World -> World
explosionUpdateSurfacePlayersUpdate w = over (players . each . object . pos) (putOn' (w ^. surface)) w

tick :: Float -> World -> IO World
tick df w@(World _ _ _ _ _ (WorldStatus _ WSS_PLAYER_INPUT) ks _ _) = do
  let keyPressedTickHandler f k (PressedKeyState t) = (if t > 0.1 && (t + df) / 0.2 > t / 0.2 then playerControlsModify k . f else f, PressedKeyState (t + df))
      (c, ks') = Map.mapAccumRWithKey keyPressedTickHandler id ks
  return $ set keysPressed ks' . c $ w
tick f w@(World s _ (Just p) _ _ (WorldStatus _ WSS_TURN_IN_PROGRESS) _ _ _) =
  let (c, p') = projectileTick f s p
   in return $ case c of
        Just c' -> set projectile Nothing . set explosion (Just $ Explosion c' 0 10) $ w
        Nothing -> set projectile p' w
tick f w@(World s _ Nothing (Just e@(Explosion c@(x, y) r mr)) _ (WorldStatus _ WSS_TURN_IN_PROGRESS) ks _ _)
  | r < mr = return $ over (explosion . _Just . radius) ((f * animationSpeed) +) w
  | otherwise = return $ set explosion Nothing . explosionUpdateSurfacePlayersUpdate . over surface (`explosionUpdateSurface` e) . explosionCheckPlayerHit . explosionAddToHistory $ w
tick f w@(World _ _ Nothing Nothing _ (WorldStatus _ WSS_TURN_IN_PROGRESS) _ _ _) = return $ nextPlayerMove w

---

historySymbols :: [Picture]
historySymbols =
  [ circle 5,
    line [(-5, -5), (5, 5)] <> line [(-5, 5), (5, -5)],
    line [(-5, 0), (5, 0)] <> line [(0, -5), (0, 5)]
  ]

historyPictures :: [Picture]
historyPictures = Pictures <$> filter (liftM2 (&&) (> 0) (<= 2) . length) (subsequences historySymbols)

historyPictures' :: [Picture]
historyPictures' = cycle historyPictures

main :: IO ()
main = do
  let mx = 1000 :: Int
      my = 1000 :: Int
      windowSize = (mx, my)
      transformer = translate (-fromIntegral mx / 2) (-fromIntegral my / 2)
      surface = Surface my mx $ Map.fromSet (sinSurface mx my) (Set.fromList $ take (mx + 1) [0 ..])
      player1 = Player (PlayerObject (putOn' surface (250, 0)) red) (PlayerControls (wrap 60) (wrap 74)) 100
      player2 = Player (PlayerObject (putOn' surface (750, 0)) blue) (PlayerControls (wrap (90 - 75 + 90)) (wrap 50)) 100
      world :: World = World surface (Map.fromList [(1, player1), (2, player2)]) Nothing Nothing [] (WorldStatus 1 WSS_PLAYER_INPUT) Map.empty transformer historyPictures'

  print $ length historyPictures
  print surface

  playIO
    (InWindow "GameEvent" windowSize (10, 10))
    white
    60
    world
    (return . render)
    eventHandler
    tick