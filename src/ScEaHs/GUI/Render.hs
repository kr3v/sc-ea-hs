{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module ScEaHs.GUI.Render where

import Control.Lens (imap, view)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe, maybeToList)
import Graphics.Gloss.Data.Color (black, greyN, orange)
import Graphics.Gloss.Data.Picture (Picture (..), circleSolid, color, rectangleSolid, scale, text, translate)
import qualified ScEaHs.GUI.Player as GUI
import ScEaHs.GUI.Player.Controls (PlayerControls (..))
import ScEaHs.GUI.World (ProjectileHistory (..), ProjectileHit (..), player, player')
import qualified ScEaHs.GUI.World as GUI
import ScEaHs.Game.Projectile (Projectile (..))
import ScEaHs.Game.Surface (Surface (..))
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator (..))
import ScEaHs.Game.World (Explosion (Explosion), Player (Player), SStatus (WSS_PLAYER_INPUT), Status (Status))
import qualified ScEaHs.Game.World as Game
import qualified ScEaHs.Game.World as World
import ScEaHs.Utils.BoundedPlus (BoundedPlus (..))
import ScEaHs.Utils.Format (showF2)

class Renderable a where
  render :: a -> Picture

class TextualInfo a where
  info :: a -> String

instance Renderable GUI.World where
  render :: GUI.World -> Picture
  render w@(GUI.World (Game.World (SurfaceWithGenerator s _ _) ps p ea st@(Status p' s') sc) t pc _ (ProjectileHistory ph _ _)) =
    let surface = render s
        projectile = render <$> maybeToList p
        explosion = maybeToList $ render <$> ea
        players = Map.elems $ render <$> GUI.players w
        projectileHits = render <$> ph
        game = Pictures $ surface : players ++ projectile ++ explosion ++ projectileHits

        text' x y s = translate x y $ scale 0.2 0.2 $ text s
        guiPlayers = GUI.players w
        playersStatus = (\(i, p) -> text' 5 (-25 - 30 * fromIntegral i) $ "  player " ++ show i ++ ": " ++ info p) <$> Map.assocs guiPlayers
        status = text' 5 (-25) $ info st ++ "; " ++ show sc
        debugInfo = text' 5 (-25 - 30 * 3) $ "explosion: " ++ show ea ++ ", projectile: " ++ show p
        debugInfo' = translate 0 1000 $ Pictures $ status : debugInfo : playersStatus

        projectileHitDebugInfo :: ProjectileHit -> Picture
        projectileHitDebugInfo (ProjectileHit _ c p i) = text' 5 0 ("hit #" ++ show i ++ ": " ++ info c) <> translate (-10) 7 p

        projectileHitsDebugInfo = translate (-450) 1000 $ Pictures $ imap (\i ph -> translate 0 (-30 * fromIntegral i) $ projectileHitDebugInfo ph) ph
     in t $ Pictures [game, debugInfo', projectileHitsDebugInfo, debugInfo']

instance TextualInfo Status where
  info :: Status -> String
  info (Status p s) = "player: " ++ show p ++ ", state: " ++ show s ++ (if s == WSS_PLAYER_INPUT then " (press space to make a turn)" else "")

instance Renderable Surface where
  render :: Surface -> Picture
  render (Surface _ _ hs) = Pictures $ imap (\x h -> translate (fromIntegral x) (fromIntegral (h `div` 2)) $ rectangleSolid 1 (fromIntegral h)) $ Map.elems hs

instance TextualInfo PlayerControls where
  info :: PlayerControls -> String
  info (PlayerControls a s) = show a ++ ", " ++ show s

instance Renderable Game.Player where
  render :: Game.Player -> Picture
  render (Game.Player (x, y) c _) = color c $ translate x y $ rectangleSolid 10 10

instance Renderable GUI.Player where
  render :: GUI.Player -> Picture
  render (GUI.Player o@(Game.Player (x, y) _ _) (PlayerControls a s)) =
    let z x y = color (greyN 0.5) $ translate x y $ circleSolid 2
        a' = fromIntegral (unwrap a) * pi / 180
        step = 10
        ps = foldr (<>) (render o) $ (\i -> z (x + i * step * cos a') (y + i * step * sin a')) . fromIntegral <$> [1 .. (unwrap s `div` round step)]
     in ps

instance TextualInfo GUI.Player where
  info :: GUI.Player -> String
  info (GUI.Player p c) = info c ++ ", health: " ++ showF2 (view World.health p)

instance Renderable Projectile where
  render :: Projectile -> Picture
  render (Projectile (x, y) _ _) = color black $ translate x y $ rectangleSolid 5 5

instance Renderable Explosion where
  render :: Explosion -> Picture
  render (Explosion (x, y) r mr) = color orange $ translate x y $ circleSolid r

instance Renderable ProjectileHit where
  render :: ProjectileHit -> Picture
  render (ProjectileHit p cs pic _) = uncurry translate p pic
