{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module ScEaHs.GUI.Render where

import Control.Lens (imap, use, view)
import Control.Monad.State (State)
import Data.Generics.Product (HasType (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Graphics.Gloss.Data.Color (black, greyN, orange)
import Graphics.Gloss.Data.Picture (Picture (..), circleSolid, color, rectangleSolid, scale, text, translate)
import ScEaHs.GUI.Plugins.Controls (ControlsPlugin, ControlsPluginState, PlayerControls (..), playersControls)
import ScEaHs.GUI.Plugins.History (HistoryPlugin, HistoryPluginState, ProjectileHistory, ProjectileHit (..), history, hits)
import ScEaHs.Game.Projectile (Projectile (..))
import ScEaHs.Game.Surface (Surface (..))
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator (..))
import ScEaHs.Game.World (Explosion (Explosion), Player (Player), SStatus (WSS_PLAYER_INPUT), Status (Status), World (..), players)
import qualified ScEaHs.Game.World as Game
import qualified ScEaHs.Game.World as World
import ScEaHs.Plugin (GamePlugin, GamePluginStateR)
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))
import ScEaHs.Utils.Format (showF2, showTF2)

class Renderable a where
  render :: a -> Picture

class RenderableS a s where
  renderS :: a -> GamePluginStateR a s Picture

class TextualInfo a where
  info :: a -> String

text' :: Float -> Float -> String -> Picture
text' x y s = translate x y $ scale 0.2 0.2 $ text s

instance Renderable Game.World where
  render :: Game.World -> Picture
  render w@(Game.World {_surfaceG = SurfaceWithGenerator {_surface = s}, _players = ps, _explosion = ea, _projectile = p, _status = st, _score = sc}) =
    let surface = render s
        projectile = render <$> maybeToList p
        explosion = maybeToList $ render <$> ea
        players = Map.elems $ render <$> ps
        game = Pictures $ surface : projectile ++ explosion

        text' x y s = translate x y $ scale 0.2 0.2 $ text s
        status = text' 5 (-25) $ info st ++ "; " ++ show sc
        debugInfo = text' 5 (-25 - 30 * 3) $ "explosion: " ++ show ea ++ ", projectile: " ++ show p
        debugInfo' = translate 0 1000 $ Pictures [status, debugInfo]
     in Pictures $ [game, debugInfo', debugInfo'] ++ players

instance TextualInfo Status where
  info :: Status -> String
  info (Status p s) = "player: " ++ show p ++ ", state: " ++ show s ++ (if s == WSS_PLAYER_INPUT then " (press space to make a turn)" else "")

instance Renderable Surface where
  render :: Surface -> Picture
  render (Surface _ _ hs) = Pictures $ imap (\x h -> translate (fromIntegral x) (fromIntegral (h `div` 2)) $ rectangleSolid 1 (fromIntegral h)) $ Map.elems hs

instance Renderable Game.Player where
  render :: Game.Player -> Picture
  render (Game.Player (x, y) c _) = color c $ translate x y $ rectangleSolid 10 10

-- instance Renderable GUI.Player where
--   render :: GUI.Player -> Picture
--   render (GUI.Player o@(Game.Player (x, y) _ _) (PlayerControls a s)) =
--      in ps

-- instance TextualInfo GUI.Player where
--   info :: GUI.Player -> String
--   info (GUI.Player p c) = info c ++ ", health: " ++ showF2 (view World.health p)

instance Renderable Projectile where
  render :: Projectile -> Picture
  render (Projectile (x, y) _ _) = color black $ translate x y $ rectangleSolid 5 5

instance Renderable Explosion where
  render :: Explosion -> Picture
  render (Explosion (x, y) r mr _) = color orange $ translate x y $ circleSolid r

instance Renderable ProjectileHit where
  render :: ProjectileHit -> Picture
  render (ProjectileHit p cs pic _) = uncurry translate p pic

instance Renderable ProjectileHistory where
  render :: ProjectileHistory -> Picture
  render h =
    let hitsA = view hits h
        hitsP = render <$> hitsA
        projectileHitDebugInfo :: ProjectileHit -> Picture
        projectileHitDebugInfo (ProjectileHit _ c p i) = text' 5 0 ("hit #" ++ show i ++ ": " ++ showTF2 c) <> translate (-10) 7 p
        debugInfo = translate (-450) 1000 $ Pictures $ imap (\i ph -> translate 0 (-30 * fromIntegral i) $ projectileHitDebugInfo ph) hitsA
     in Pictures (debugInfo : hitsP)

instance Renderable HistoryPlugin where
  render :: HistoryPlugin -> Picture
  render p = render $ view history p

instance RenderableS ControlsPlugin s where
  renderS :: ControlsPlugin -> GamePluginStateR ControlsPlugin s Picture
  renderS _ = do
    let projections (Game.Player (x, y) _ _) (PlayerControls a s) =
          let z x y = color (greyN 0.5) $ translate x y $ circleSolid 2
              a' = fromIntegral (unwrap a) * pi / 180
              step = 10
           in (\i -> z (x + i * step * cos a') (y + i * step * sin a')) . fromIntegral <$> [1 .. (unwrap s `div` round step)]
        debugInfo i (PlayerControls a s) = text' 5 (-25 - 30 * fromIntegral i) $ "  player " ++ show i ++ ": " ++ show a ++ ", " ++ show s
    players <- use (typed @Game.World . players)
    controls <- use (typed @ControlsPlugin . playersControls)
    let playersWithControls = Map.intersectionWith (,) players controls

    let debugInfoP = Pictures $ uncurry debugInfo <$> Map.assocs controls
    let projectionsP = Pictures $ concatMap (uncurry projections) (Map.elems playersWithControls)
    return $ debugInfoP <> projectionsP