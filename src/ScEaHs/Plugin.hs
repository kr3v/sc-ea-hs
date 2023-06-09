{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ScEaHs.Plugin where

import Control.Lens (Bifunctor (bimap), Identity, use)
import Control.Monad.State (MonadState (..), MonadTrans (..), State, StateT, execState, execStateT, liftM, liftM2, runState)
import Control.Monad.Trans.MultiState (MultiState)
import Data.Data (Proxy (..))
import Data.Generics.Product (HasType (..))
import Graphics.Gloss (Picture)
import Graphics.Gloss.Interface.IO.Game (Event)
import qualified ScEaHs.Game.World as Game

type GamePluginStateR a s r = (GamePlugin a, HasType a s, HasType Game.World s) => State s r

type GamePluginState a s = GamePluginStateR a s ()

class GamePlugin a where
  event :: Game.World -> a -> Event -> GamePluginState a s
  tick :: Game.World -> a -> Float -> GamePluginState a s

tickP :: forall a s. (GamePlugin a) => Proxy a -> Float -> GamePluginState a s
tickP _ df = do
  a <- use (typed @a)
  w <- use (typed @Game.World)
  tick w a df

eventP :: forall a s. (GamePlugin a) => Proxy a -> Event -> GamePluginState a s
eventP _ e = do
  a <- use (typed @a)
  w <- use (typed @Game.World)
  event w a e
