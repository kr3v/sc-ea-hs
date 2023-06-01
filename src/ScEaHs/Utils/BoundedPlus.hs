{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ScEaHs.Utils.BoundedPlus (BoundedPlus, bound, unwrap, wrap, (<+>)) where

import Data.Coerce (Coercible, coerce)
import Data.Proxy

class Coercible a Int => BoundedPlus a where
  bound :: Proxy a -> Int

  unwrap :: a -> Int
  unwrap = coerce

  wrap :: Int -> a
  wrap = coerce . (`mod` bound (Proxy @a))

  (<+>) :: a -> Int -> a
  (<+>) a b = wrap $ (unwrap a + b) `mod` bound (Proxy @a)

(<+!>) :: Int -> Int -> Int -> Int
(<+!>) m a b = (a + b) `mod` m