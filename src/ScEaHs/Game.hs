module ScEaHs.Game where

import Control.Lens (Bifunctor (bimap), Each (each), Zoom (zoom), over, set, to, use, view, (%=), (%~), (&), (.=), _Just)
import Control.Lens.Prism (_Just)
import Control.Monad.State (State, execState, when)
import Data.List (find, uncons, unfoldr)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Graphics.Gloss (Point)
import ScEaHs.Game.Explosion (Explosion (..), radius)
import ScEaHs.Game.Explosion.Logic (explosionApplyHitIfNecessary, explosionUpdateSurface)
import ScEaHs.Game.Projectile (Projectile (..), ProjectileSource (..), projectileTick)
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

explosionCheckPlayersHit :: State Game.World ()
explosionCheckPlayersHit = do
  e <- fromJust <$> use explosion
  zoom (players . each) (explosionApplyHitIfNecessary e)

animationSpeed :: Float
animationSpeed = 10

gravityForce :: Float
gravityForce = 10

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
  | otherwise = set explosion Nothing . execState putPlayersOnSurface . over (surfaceG . surface) (explosionUpdateSurface e) . execState explosionCheckPlayersHit $ w
tick1 df w@(Game.World {_players = players, _explosion = Nothing, _projectile = Nothing, _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}}) =
  let ls = losers w
   in if not $ Map.null ls
        then execState (weveGotAWinner ls) w
        else w & status %~ execState nextPlayerMove
tick1 df w = w
