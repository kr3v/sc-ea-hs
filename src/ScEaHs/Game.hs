{-# LANGUAGE TupleSections #-}

module ScEaHs.Game where

import Control.Lens (Bifunctor (bimap), Each (each), Zoom (zoom), over, set, use, view, (%=), (%~), (&), (.=), _Just)
import Control.Lens.Prism (_Just)
import Control.Monad.State (State, execState)
import Data.List (find, uncons, unfoldr)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Graphics.Gloss (Point)
import ScEaHs.Game.Projectile (Explosion (..), Projectile (..), ProjectileSource (..), radius)
import ScEaHs.Game.Surface (Surface, heights, isWithinSurface, putOn')
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator (..), surface, updateSurface)
import ScEaHs.Game.World (SStatus (WSS_PLAYER_INPUT, WSS_TURN_IN_PROGRESS), Status (_wstatus), World (_explosion, _players, _projectile, _status, _surfaceG), explosion, health, players, pos, projectile, score, status, surfaceG, turn, wstatus)
import qualified ScEaHs.Game.World as Game
import ScEaHs.Utils.Geometry (distance, intersectionCircleVerticalLine)

nextPlayerMove :: State Game.Status ()
nextPlayerMove = do
  t <- use turn
  turn .= if t == 1 then 2 else 1
  wstatus .= WSS_PLAYER_INPUT

projectileLaunch :: Projectile -> State Game.World ()
projectileLaunch p = do
  status . wstatus .= WSS_TURN_IN_PROGRESS
  projectile .= Just p

produce :: Float -> Float -> [Float]
produce x1 x2 = concat $ unfoldr (\x -> if x <= x2 then Just ([x - 0.01, x + 0.01], x + 1) else Nothing) (fromIntegral $ floor x1)

animationSpeed :: Float
animationSpeed = 10

gravityForce :: Float
gravityForce = 10

projectileTick :: Float -> Surface -> Projectile -> (Maybe Point, Maybe Projectile)
projectileTick df s (Projectile (x0, y0) (vx, vy) t src) =
  let dx = vx * animationSpeed * df
      dy = vy * animationSpeed * df
      x1 = x0 + dx
      y1 = y0 + dy
      p = Projectile (x1, y1) (vx, vy - gravityForce * animationSpeed * df) t src
      xs = filter (isWithinSurface s) $ produce (min x0 x1) (max x0 x1)
      ys = (\x -> if dx < 0.01 then y0 else (x - x0) / dx * dy + y0) <$> xs
      hs = (\x -> snd $ putOn' s (x, 0)) <$> xs
      collision = (\(x, _, h) -> (x, h)) <$> find (\(_, y, h) -> y < h) (zip3 xs ys hs)
   in (collision, if isWithinSurface s x1 then Just p else Nothing)

explosionUpdateSurface :: Explosion -> Surface -> Surface
explosionUpdateSurface (Explosion e@(x, y) r mr _) s =
  let xs = filter (isWithinSurface s) [x - mr .. x + mr]
      hs = (\x -> snd $ putOn' s (x, 0)) <$> xs

      decide h x' ((x1, y1), (x2, y2))
        | y2 < h = h - (y2 - y1)
        | y1 < h = y1
        | otherwise = h
      decide' (h, x, i12) = (x, max (decide h x i12) 5)

      z = Map.fromList (bimap round round <$> (decide' <$> catMaybes (zipWith (\h x -> (h,x,) <$> intersectionCircleVerticalLine e mr x) hs xs)))
   in over heights (Map.union z) s

-- todo: consider higher d hp / consider extending explosion radius
explosionCheckPlayerHit' :: Explosion -> Game.Player -> Game.Player
explosionCheckPlayerHit' (Explosion c _ mr _) p =
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

weveGotAWinner :: Map.Map Int Game.Player -> State Game.World ()
weveGotAWinner losers = do
  zoom surfaceG updateSurface
  zoom status nextPlayerMove
  putPlayersOnSurface
  players . each . health .= 100
  score %= Map.unionWith (+) (-1 <$ losers)

losers :: Game.World -> Map.Map Int Game.Player
losers w = Map.filter ((<= 0) . view health) $ view players w

tick1 :: Float -> Game.World -> Game.World
tick1 df w@(Game.World {_surfaceG = SurfaceWithGenerator {_surface = s}, _projectile = (Just p@Projectile {_psrc = psrc}), _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}) =
  let (c, p') = projectileTick df s p
   in case c of
        Just c' -> set projectile Nothing . set explosion (Just $ Explosion c' 0 10 psrc) $ w
        Nothing -> set projectile p' w
tick1 df w@(Game.World {_surfaceG = SurfaceWithGenerator {_surface = s}, _explosion = Just e@(Explosion c@(x, y) r mr _), _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}})
  | r < mr = over (explosion . _Just . radius) ((df * animationSpeed) +) w
  | otherwise = set explosion Nothing . execState putPlayersOnSurface . over (surfaceG . surface) (explosionUpdateSurface e) . explosionCheckPlayerHit $ w
tick1 df w@(Game.World {_players = players, _explosion = Nothing, _projectile = Nothing, _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}) =
  let ls = losers w
   in if not $ Map.null ls
        then execState (weveGotAWinner ls) w
        else w & status %~ execState nextPlayerMove
tick1 df w = w
