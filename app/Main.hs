-- suggestion to copilot: I'm writing a scorched earth game
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens (Lens', makeLenses, over, set, view, (%~), (&), (.~), (^.))
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.List (unfoldr)
import Data.List.Index (imap)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import GHC.RTS.Flags (TraceFlags (traceScheduler))
import Graphics.Gloss.Data.Color (Color, blue, greyN, red, white)
import Graphics.Gloss.Data.Display (Display (InWindow))
import Graphics.Gloss.Data.Picture (Picture (..), blank, color, rectangleSolid, scale, text, translate)
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Vector (Vector)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), Modifiers, MouseButton (..), SpecialKey (..), black, playIO)
import Graphics.Gloss.Interface.Pure.Game (play)

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
  bound _ = 100
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

data WorldStatusState = WSS_PLAYER_INPUT | WSS_TURN_IN_PROGRESS deriving (Show)

data WorldStatus = WorldStatus
  { _turn :: Int,
    _state :: WorldStatusState
  }
  deriving (Show)

data Surface = Surface
  { _maxHeight :: Int,
    _width :: Int,
    _heights :: [Int]
  }
  deriving (Show)

data World = World
  { _surface :: Surface,
    _players :: Map.Map Int Player,
    _projectile :: Maybe Projectile,
    _status :: WorldStatus,
    _transformer :: Picture -> Picture
  }

data PlayerControls = PlayerControls
  { _angle :: Angle,
    _str :: Strength
  }
  deriving (Show)

data PlayerObject = PlayerObject
  { _pos :: Point,
    _color :: Color
  }
  deriving (Show)

data Player = Player
  { _object :: PlayerObject,
    _controls :: PlayerControls
  }
  deriving (Show)

data ProjectileType = SHELL deriving (Show)

data Projectile = Projectile
  { _ppos :: Point,
    _speed :: Vector,
    _ptype :: ProjectileType
  }
  deriving (Show)

$(makeLenses ''World)
$(makeLenses ''WorldStatus)
$(makeLenses ''Surface)
$(makeLenses ''PlayerControls)
$(makeLenses ''PlayerObject)
$(makeLenses ''Player)
$(makeLenses ''Projectile)

---

instance Renderable World where
  render :: World -> Picture
  render w@(World s ps p st@(WorldStatus p' s') t) =
    let surface = render s
        pp = maybeToList $ render <$> p
        playersPictures = render <$> ps
        playersStatus = info <$> ps
        status = info st
        text' x y s = translate x y $ scale 0.2 0.2 $ text s
        playersStatus' = (\(i, p) -> text' 5 (1000 - 25 - 30 * fromIntegral i) $ "  player " ++ show i ++ ": " ++ show (view controls p)) <$> Map.assocs ps
        status' = text' 5 (1000 - 25) status
     in t $ Pictures $ surface : status' : Map.elems playersPictures ++ playersStatus' ++ pp

instance TextualInfo WorldStatus where
  info :: WorldStatus -> String
  info (WorldStatus p s) = "player: " ++ show p ++ ", state: " ++ show s

instance Renderable Surface where
  render :: Surface -> Picture
  render (Surface _ _ hs) = Pictures $ imap (\x h -> translate (fromIntegral x) (fromIntegral (h `div` 2)) $ rectangleSolid 1 (fromIntegral h)) hs

instance TextualInfo PlayerControls where
  info :: PlayerControls -> String
  info (PlayerControls a s) = "angle: " ++ show a ++ ", strength: " ++ show s

instance Renderable PlayerObject where
  render :: PlayerObject -> Picture
  render (PlayerObject (x, y) c) = Graphics.Gloss.Data.Picture.color c $ translate x y $ rectangleSolid 10 10

instance Renderable Player where
  render :: Player -> Picture
  render (Player o c) = render o

instance TextualInfo Player where
  info :: Player -> String
  info (Player o c) = info c

instance Renderable Projectile where
  render :: Projectile -> Picture
  render (Projectile (x, y) _ _) = Graphics.Gloss.Data.Picture.color black $ translate x y $ rectangleSolid 5 5

---

transformPicture :: Vector -> Picture -> Picture
transformPicture (dx, dy) = translate dx dy

putOn :: Surface -> Point -> Point
putOn (Surface mh mw hs) (x, y) = (x, fromIntegral $ hs !! round x)

sinSurface :: Int -> Int -> Int -> Int
sinSurface mx my x =
  let x' = fromIntegral x :: Float
      mx' = fromIntegral mx :: Float
      my' = fromIntegral my :: Float
      x'_norm = x' / mx'
      x'_norm_2pi = x'_norm * 2 * pi
      h_var0 = sin x'_norm_2pi * (my' / 4)
      h_var1 = sin (x'_norm_2pi * 2) * (my' / 8)
      h_var2 = sin (x'_norm_2pi * 4) * (my' / 16)
      h_var3 = sin (x'_norm_2pi * 8) * (my' / 32)
      h_var4 = sin (x'_norm_2pi * 16) * (my' / 64)
      h = round (h_var0 + h_var1 + h_var2 + h_var3 + h_var4 + my' / 3)
   in h

---

modify :: BoundedPlus b => Lens' PlayerControls b -> Int -> Player -> Player
modify a d = over (controls . a) (<+> d)

playerKeyHandler :: SpecialKey -> Player -> Player
playerKeyHandler KeyLeft = modify angle (-1)
playerKeyHandler KeyRight = modify angle 1
playerKeyHandler KeyDown = modify str (-1)
playerKeyHandler KeyUp = modify str 1
playerKeyHandler _ = id

spaceKeyHandler :: World -> World
spaceKeyHandler w@(World s ps p st@(WorldStatus p' s') t) =
  let Player (PlayerObject pos c) (PlayerControls a s) = ps Map.! p'
      s' :: Float = fromIntegral (unwrap s)
      a' :: Float = fromIntegral (unwrap a) * pi / 180
      vx = s' * cos a'
      vy = s' * sin a'
      proj = Projectile pos (vx, vy) SHELL
   in (over status (set state WSS_TURN_IN_PROGRESS) . set projectile (Just proj)) w

worldEventHandler :: Event -> World -> IO World
worldEventHandler e w@(World s ps p st@(WorldStatus p' s') t) =
  case s' of
    WSS_TURN_IN_PROGRESS -> return w
    WSS_PLAYER_INPUT -> case e of
      EventKey (SpecialKey KeySpace) Down m (x, y) -> return $ spaceKeyHandler w
      EventKey (SpecialKey c) Down m (x, y) -> return $ over players (Map.update (Just . playerKeyHandler c) p') w
      EventKey (SpecialKey c) Up m (x, y) -> return w
      EventKey (Char c) ks m (x, y) -> return w
      EventMotion (x, y) -> return w
      _ -> print e >> return w

produce :: Float -> Float -> [Float]
produce x1 x2 = concat $ unfoldr (\x -> if x < x2 then Just ([x - 0.01, x + 0.01], x + 1) else Nothing) (fromIntegral $ floor x1 + 1)

iterateProjectile :: Float -> Surface -> Projectile -> (Maybe Point, Projectile)
iterateProjectile f s (Projectile (x0, y0) (vx, vy) t) =
  let dx = vx * f
      dy = vy * f
      x1 = x0 + dx
      y1 = y0 + dy
      p = Projectile (x1, y1) (vx, vy - 10 * f) t
      xs = produce x0 x1
      ys = (\x -> (x - x0) / dx * dy + y0) <$> xs
      heights = (\x -> snd $ putOn s (x, 0)) <$> xs
      collision = find (uncurry (<)) (zip ys heights)
   in (collision, p)
   
-- over status (set state WSS_PLAYER_INPUT . set turn (if t == 1 then 2 else 1)) w
handleProjectileCollision :: Point -> World -> World
handleProjectileCollision c w@(World s ps (Just p) st@(WorldStatus t WSS_TURN_IN_PROGRESS) _) = over status (set state WSS_PLAYER_INPUT . set turn (if t == 1 then 2 else 1)) w

worldTickHandler :: Float -> World -> IO World
worldTickHandler f w@(World _ _ _ (WorldStatus _ WSS_PLAYER_INPUT) _) = return w
worldTickHandler f w@(World _ _ Nothing (WorldStatus _ WSS_TURN_IN_PROGRESS) _) = return $ over status (set state WSS_PLAYER_INPUT) w
worldTickHandler f w@(World s _ (Just p) st@(WorldStatus _ WSS_TURN_IN_PROGRESS) _) =
  let (c, p') = iterateProjectile f s p
   in return $ case c of
        Just p -> handleProjectileCollision p w
        Nothing -> set projectile (Just p') w

--

-- TODO:
-- refactoring
-- implement player controls

main :: IO ()
main = do
  let mx = 1000 :: Int
      my = 1000 :: Int
      emptyControls = PlayerControls (wrap 75) (wrap 50)
      windowSize = (mx, my)
      transformer = transformPicture (-fromIntegral mx / 2, -fromIntegral my / 2)
      surface = Surface my mx $ sinSurface mx my <$> take (mx + 1) [0 ..]
      player1 = Player (PlayerObject (putOn surface (250, 0)) red) emptyControls
      player2 = Player (PlayerObject (putOn surface (750, 0)) blue) emptyControls
      world :: World = World surface (Map.fromList [(1, player1), (2, player2)]) Nothing (WorldStatus 1 WSS_PLAYER_INPUT) transformer

  print surface

  playIO
    (InWindow "GameEvent" windowSize (10, 10))
    white
    60
    world
    (return . render)
    worldEventHandler
    worldTickHandler