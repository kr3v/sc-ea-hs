{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ScEaHs.GUI.Plugins.History where

import Control.Lens (Zoom (zoom), makeLenses, use, (%%=), (%=), (%~), (&), (.=), (<<%=), (^.))
import Control.Monad (liftM2)
import Control.Monad.State (MonadState (..), MonadTrans (..), State, StateT)
import Control.Monad.Trans.MultiState (MonadMultiGet (..), MonadMultiState (..), MultiState)
import Data.Generics.Product (HasType (typed))
import Data.List (subsequences)
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Graphics.Gloss (Color, Picture (..), Point, blank, circle, color, line, translate)
import Graphics.Gloss.Interface.IO.Game (Event)
import ScEaHs.Game (losers)
import ScEaHs.Game.Explosion (Explosion (..))
import ScEaHs.Game.Projectile (ProjectileSource (..))
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator (..))
import ScEaHs.Game.World (SStatus (..), Status (..), World (..), pcolor)
import qualified ScEaHs.Game.World as Game
import ScEaHs.Plugin (GamePlugin (..), GamePluginState)
import ScEaHs.Utils.List (uncons')

symbols :: [Picture]
symbols =
  [ translate (-0.4) (-0.4) $ circle 4,
    line [(-5, -5), (5, 5)] <> line [(-5, 5), (5, -5)], -- x
    line [(-5, 0), (5, 0)] <> line [(0, -5), (0, 5)], -- +
    line [(-5, -5), (-5, 5)] <> line [(5, -5), (5, 5)] <> line [(-5, -5), (5, -5)] <> line [(-5, 5), (5, 5)], -- square
    line [(-5, 0), (0, 5)] <> line [(0, 5), (5, 0)] <> line [(5, 0), (0, -5)] <> line [(0, -5), (-5, 0)], -- diamond
    line [(-5, -5), (5, -5)] <> line [(5, -5), (0, 5)] <> line [(0, 5), (-5, -5)] -- triangle
  ]

symbolsPictures' :: [Picture]
symbolsPictures' = Pictures <$> filter (liftM2 (&&) (> 0) (<= 2) . length) (subsequences symbols)

symbolsPictures :: [Picture]
symbolsPictures = cycle symbolsPictures'

---

data ProjectileHit = ProjectileHit
  { _hpos :: Point,
    _cs :: (Float, Float),
    _pic :: Picture,
    _idx :: Int
  }

data ProjectileHistory = ProjectileHistory
  { _hits :: [ProjectileHit],
    _pictures :: [Picture],
    _picturesIdx :: Int
  }

$(makeLenses ''ProjectileHit)
$(makeLenses ''ProjectileHistory)

recordExplosion :: Explosion -> Color -> State ProjectileHistory ()
recordExplosion (Explosion {_epos = p, _src = ProjectileSource {_icontrols = cnt}}) c = do
  hs <- pictures %%= uncons'
  idx <- picturesIdx <<%= (+ 1)
  let ph = ProjectileHit p cnt (color c hs) idx
  hits %= (ph :)

newtype HistoryPlugin = HistoryPlugin
  { _history :: ProjectileHistory
  }
  deriving (Generic)

$(makeLenses ''HistoryPlugin)

type HistoryPluginState s = GamePluginState HistoryPlugin s

instance GamePlugin HistoryPlugin where
  event :: World -> HistoryPlugin -> Event -> GamePluginState HistoryPlugin s
  event _ _ _ = return ()

  tick :: World -> HistoryPlugin -> Float -> GamePluginState HistoryPlugin s
  tick w@Game.World {_explosion = Just e@(Explosion {_radius = r, _maxRadius = mr})} _ df | r >= mr = do
    let p = Game.currentPlayer w
    zoom (typed @HistoryPlugin . history) (recordExplosion e (fromJust p ^. pcolor))
  tick w@Game.World {_explosion = Nothing, _projectile = Nothing, _status = Game.Status {_wstatus = WSS_TURN_IN_PROGRESS}} _ df | not $ null $ losers w = do
    typed @HistoryPlugin . history . hits .= []
  tick _ _ _ = return ()