-- suggestion to copilot: I'm writing a scorched earth game
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Lens (At (at), Bifunctor (bimap), Each (each), Lens', makeLenses, non, over, set, use, view, (%%=), (%%~), (%=), (%~), (&), (.=), (.~), (<%=), (<.=), (<<%=), (^.))
import Control.Lens.Lens (Lens', (&), (<<%~))
import Control.Lens.Prism (_Just)
import Control.Monad (liftM2)
import Control.Monad.State (MonadState (get, put), State, execState, gets, modify, runState, state)
import Data.Fixed ()
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.IORef (newIORef)
import Data.List (subsequences, unfoldr)
import Data.List.Index (imap)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, maybeToList)
import qualified Data.Set as Set
import Data.Time (secondsToNominalDiffTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.RTS.Flags (ProfFlags (doHeapProfile), TraceFlags (traceScheduler))
import Graphics.Gloss.Data.Color (Color, blue, greyN, orange, red, white)
import Graphics.Gloss.Data.Display (Display (InWindow))
import Graphics.Gloss.Data.Picture (Picture (..), blank, circle, color, line, rectangleSolid, rotate, scale, text, translate)
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Vector (Vector)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), Modifiers, MouseButton (..), SpecialKey (..), black, circleSolid, playIO)
import Graphics.Gloss.Interface.Pure.Game (play)
import Numeric (showFFloat)
import System.Random (StdGen, mkStdGen)
import System.Random.Stateful (Random (randomR), StdGen, newStdGen)

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

data WorldState = WSS_PLAYER_INPUT | WSS_TURN_IN_PROGRESS deriving (Show, Eq)

data WorldStatus = WorldStatus
  { _turn :: Int,
    _wstatus :: WorldState
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
    _projectileHitPictures :: [Picture],
    _projectileHitPicturesIdx :: Int,
    _surfaceStdGen :: StdGen,
    _surfaceState :: StdGen -> (Surface, StdGen),
    _score :: Map.Map Int Int
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
    _cs :: PlayerControls,
    _pic :: Picture,
    _idx :: Int
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
  render w@(World s ps p ea ph st@(WorldStatus p' s') _ t _ _ _ _ sc) =
    let surface = render s
        projectile = render <$> maybeToList p
        explosion = maybeToList $ render <$> ea
        players = Map.elems $ render <$> ps
        projectileHits = render <$> ph
        game = Pictures $ surface : players ++ projectile ++ explosion ++ projectileHits

        text' x y s = translate x y $ scale 0.2 0.2 $ text s
        playersStatus = (\(i, p) -> text' 5 (-25 - 30 * fromIntegral i) $ "  player " ++ show i ++ ": " ++ info p) <$> Map.assocs ps
        status = text' 5 (-25) $ info st ++ "; " ++ show sc
        debugInfo = text' 5 (-25 - 30 * 3) $ "explosion: " ++ show ea ++ ", projectile: " ++ show p
        debugInfo' = translate 0 1000 $ Pictures $ status : debugInfo : playersStatus

        projectileHitDebugInfo :: ProjectileHit -> Picture
        projectileHitDebugInfo (ProjectileHit _ c p i) = text' 5 0 ("hit #" ++ show i ++ ": " ++ info c) <> translate (-10) 7 p

        projectileHitsDebugInfo = translate (-450) 1000 $ Pictures $ imap (\i ph -> translate 0 (-30 * fromIntegral i) $ projectileHitDebugInfo ph) ph
     in t $ Pictures [game, debugInfo', projectileHitsDebugInfo, debugInfo']

instance TextualInfo WorldStatus where
  info :: WorldStatus -> String
  info (WorldStatus p s) = "player: " ++ show p ++ ", state: " ++ show s ++ (if s == WSS_PLAYER_INPUT then " (press space to make a turn)" else "")

instance Renderable Surface where
  render :: Surface -> Picture
  render (Surface _ _ hs) = Pictures $ imap (\x h -> translate (fromIntegral x) (fromIntegral (h `div` 2)) $ rectangleSolid 1 (fromIntegral h)) $ Map.elems hs

instance TextualInfo PlayerControls where
  info :: PlayerControls -> String
  info (PlayerControls a s) = show a ++ ", " ++ show s

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

instance Renderable ProjectileHit where
  render :: ProjectileHit -> Picture
  render (ProjectileHit p cs pic _) = uncurry translate p pic

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

mapGenerator :: Int -> Float -> Int -> Int -> Int -> Int
mapGenerator offset seed mx my x =
  let x' = fromIntegral (x + offset) :: Float
      mx' = fromIntegral mx :: Float
      my' = fromIntegral my :: Float
      x'_norm = x' / mx'
      x'_norm_2pi = x'_norm * 2 * pi
      h_var0 = sin x'_norm_2pi * (my' / (4 * seed))
      h_var1 = cos (x'_norm_2pi * 2) * (my' / (8 * seed))
      h_var2 = sin (x'_norm_2pi * 4) * (my' / (16 * seed))
      h_var3 = cos (x'_norm_2pi * 8) * (my' / (32 * seed))
      h_var4 = sin (x'_norm_2pi * 16) * (my' / (64 * seed))
      h = round (h_var0 + h_var1 + h_var2 + h_var3 + h_var4 + my' / 3)
   in h

isWithinSurface :: Surface -> Float -> Bool
isWithinSurface (Surface mh mw hs) x = x >= 0 && x <= fromIntegral mw

---

nextPlayerMove :: State World ()
nextPlayerMove = do
  t <- use $ status . turn
  status .= WorldStatus (if t == 1 then 2 else 1) WSS_PLAYER_INPUT

currentPlayer :: World -> Player
currentPlayer w = (w ^. players) Map.! (w ^. status . turn)

---

playerControlsModify'' :: BoundedPlus b => Lens' PlayerControls b -> Int -> Player -> Player
playerControlsModify'' a d = over (controls . a) (<+> d)

playerControlsModify' :: SpecialKey -> Player -> Player
playerControlsModify' KeyLeft = playerControlsModify'' angle 1
playerControlsModify' KeyRight = playerControlsModify'' angle (-1)
playerControlsModify' KeyDown = playerControlsModify'' str (-1)
playerControlsModify' KeyUp = playerControlsModify'' str 1
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

projectileLaunch :: State World ()
projectileLaunch = do
  p <- gets currentPlayer
  (status . wstatus) .= WSS_TURN_IN_PROGRESS
  projectile .= Just (projectileLaunch' p)
  keysPressed .= Map.empty

eventHandler :: Event -> World -> IO World
eventHandler _ w@(World _ _ _ _ _ (WorldStatus _ WSS_TURN_IN_PROGRESS) _ _ _ _ _ _ _) = return w
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

uncons' :: [a] -> (a, [a])
uncons' (x : xs) = (x, xs)

explosionAddToHistory :: World -> World
explosionAddToHistory = execState explosionAddToHistory'

explosionAddToHistory' :: State World ()
explosionAddToHistory' = do
  hs <- projectileHitPictures %%= uncons'
  idx <- projectileHitPicturesIdx <<%= (+ 1)
  (Player (PlayerObject _ pc) c _) <- gets currentPlayer
  e <- gets (fromJust . view explosion)
  let ph = ProjectileHit (e ^. epos) c (color pc hs) idx
  projectilesHistory %= (ph :)

-- todo: consider higher d hp / consider extending explosion radius
explosionCheckPlayerHit' :: World -> Player -> Player
explosionCheckPlayerHit' (World _ _ _ (Just (Explosion c _ mr)) _ _ _ _ _ _ _ _ _) p =
  let d = distance c $ view (object . pos) p
      hp_delta = if d > mr then 0 else (1 - d / mr) * 2 * 100
   in over health (flip (-) hp_delta) p

explosionCheckPlayerHit :: World -> World
explosionCheckPlayerHit w = over (players . each) (explosionCheckPlayerHit' w) w

-- todo: consider removing hp for falling down
putPlayersOnSurface :: State World ()
putPlayersOnSurface = do
  s <- use surface
  players . each . object . pos %= putOn' s

weveGotAWinner :: Map.Map Int Player -> State World ()
weveGotAWinner losers = do
  updateSurface
  nextPlayerMove
  putPlayersOnSurface
  players . each . health .= 100
  projectilesHistory .= []
  let z = -1 <$ losers
  score %= Map.unionWith (+) z


tick :: Float -> World -> IO World
tick df w@(World _ _ _ _ _ (WorldStatus _ WSS_PLAYER_INPUT) ks _ _ _ _ _ _)
  | null ks = return w
  | otherwise =
      let keyPressedTickHandler f k (PressedKeyState t) = (if t > 0.1 && (t + df) / 0.2 > t / 0.2 then playerControlsModify k . f else f, PressedKeyState (t + df))
          (c, ks') = Map.mapAccumRWithKey keyPressedTickHandler id ks
       in return $ set keysPressed ks' . c $ w
tick f w@(World s _ (Just p) _ _ (WorldStatus _ WSS_TURN_IN_PROGRESS) _ _ _ _ _ _ _) =
  let (c, p') = projectileTick f s p
   in return $ case c of
        Just c' -> set projectile Nothing . set explosion (Just $ Explosion c' 0 10) $ w
        Nothing -> set projectile p' w
tick f w@(World s _ Nothing (Just e@(Explosion c@(x, y) r mr)) _ (WorldStatus _ WSS_TURN_IN_PROGRESS) ks _ _ _ _ _ _)
  | r < mr = return $ over (explosion . _Just . radius) ((f * animationSpeed) +) w
  | otherwise = return $ set explosion Nothing . execState putPlayersOnSurface . over surface (`explosionUpdateSurface` e) . explosionCheckPlayerHit . explosionAddToHistory $ w
tick f w@(World _ ps Nothing Nothing _ (WorldStatus _ WSS_TURN_IN_PROGRESS) _ _ _ _ _ _ _) = do
  let ps' = Map.filter ((<= 0) . view health) ps
   in if Map.null ps'
        then return $ execState nextPlayerMove w
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

generateSurface :: Int -> Int -> State StdGen Surface
generateSurface mx my = do
  offset <- state $ randomR (0, 16384)
  seed <- state $ randomR (1, 10)
  let g = mapGenerator offset seed mx my
  return $ Surface my mx (Map.fromSet g (Set.fromList $ take (mx + 1) [0 ..]))

updateSurface :: State World ()
updateSurface = do
  s <- use surfaceState
  s' <- surfaceStdGen %%= s
  surface .= s'

main :: IO ()
main = do
  w <- getPOSIXTime
  let g = mkStdGen $ round w

  let mx = 1000 :: Int
      my = 1000 :: Int
      windowSize = (mx, my)
      transformer = translate (-fromIntegral mx / 2) (-fromIntegral my / 2)
      surfaceGenerator = runState $ generateSurface mx my
      (surface, w') = surfaceGenerator g
      player1 = Player (PlayerObject (putOn' surface (250, 0)) red) (PlayerControls (wrap 60) (wrap 50)) 100
      player2 = Player (PlayerObject (putOn' surface (750, 0)) blue) (PlayerControls (wrap (180 - 60)) (wrap 50)) 100
      players = Map.fromList [(1, player1), (2, player2)]
      world :: World = World surface players Nothing Nothing [] (WorldStatus 1 WSS_PLAYER_INPUT) Map.empty transformer historyPictures' 1 w' surfaceGenerator (0 <$ players)

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
