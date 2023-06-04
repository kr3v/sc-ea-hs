{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module ScEaHs.Game.Surface.GeneratorSpec where

import Control.Applicative (liftA2)
import Control.Lens ((<&>))
import Control.Monad.State (evalState, execState, liftM2, runState)
import qualified Data.Map.Strict as Map
import ScEaHs.Game.Surface (Surface (..))
import ScEaHs.Game.Surface.Generator (generateSurface, mapGenerator)
import System.Random (mkStdGen)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, Testable (property), choose, suchThat, whenFail', (==>))
import Test.QuickCheck.Property (forAll)
import Data.Ix (Ix(inRange))

validArguments_generateSurface :: Gen (Int, Int)
validArguments_generateSurface = do
  mx <- suchThat arbitrary (>= 0)
  my <- suchThat arbitrary (>= 10)
  return (mx, my)

validArguments_mapGenerator :: Gen (Int, Float, Int, Int, Int)
validArguments_mapGenerator = do
  offset <- arbitrary
  seed <- suchThat arbitrary (> 0)
  mx <- suchThat arbitrary (>= 0)
  my <- suchThat arbitrary (>= 10)
  x <- suchThat arbitrary $ liftA2 (&&) (0 <=) (mx >=)
  return (offset, seed, mx, my, x)

verifier_y :: Int -> Int -> Bool
verifier_y my = inRange (5, 4 * my `div` 5)

spec_mapGenerator :: Spec
spec_mapGenerator =
  describe "mapGenerator" $
    it "returns values within expected bounds" $
      forAll validArguments_mapGenerator $
        \a@(offset, seed, mx, my, x) ->
          let y = mapGenerator offset seed mx my x
              wf = whenFail' $ putStrLn $ show a ++ " -> " ++ show y
           in wf $ verifier_y my y

spec_generateSurface :: Spec
spec_generateSurface = do
  let stdgen = mkStdGen 0
  describe "generateSurface" $
    it "returns a surface with the expected dimensions" $
      forAll validArguments_generateSurface $
        \a@(mx, my) ->
          let sg = generateSurface mx my
              s@(Surface my' mx' d) = evalState sg stdgen
              wf = whenFail' $ putStrLn $ show a ++ " -> " ++ show s
           in wf $ my' == my && mx' == mx && all (verifier_y my) (Map.elems d) && all (inRange (0, mx)) (Map.keys d)

spec :: Spec
spec = do
    spec_mapGenerator
    spec_generateSurface