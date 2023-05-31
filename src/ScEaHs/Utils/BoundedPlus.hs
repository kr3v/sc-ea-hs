{-# LANGUAGE FlexibleContexts #-}

module ScEaHs.Utils.BoundedPlus (BoundedPlus, bound, unwrap, wrap, (<+>)) where

import Data.Coerce (Coercible, coerce)

class Coercible a Int => BoundedPlus a where
  bound :: a -> Int

  unwrap :: a -> Int
  unwrap = coerce

  wrap :: Int -> a
  wrap = coerce

  (<+>) :: a -> Int -> a
  (<+>) a b = wrap $ (unwrap a + b) `mod` bound a
