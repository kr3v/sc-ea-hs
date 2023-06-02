module ScEaHs.Utils.GeometrySpec where

import Data.Proxy (Proxy (Proxy))
import Graphics.Gloss (Point)
import ScEaHs.Utils.Geometry (distance, intersectionCircleVerticalLine)
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Arbitrary (arbitrary), Property, Testable (property), classify, collect, forAll, label, suchThat, whenFail', (==>))

eqEps :: Float -> Float -> Bool
eqEps a b = abs (a - b) < 0.01

ltEps :: Float -> Float -> Bool
ltEps a b = a < b + 0.01

prop_icvl :: Float -> Float -> Float -> Float -> Bool -> Property
prop_icvl x0 y0 r x isNothing =
  case result of
    Nothing -> label "nothing" isNothing
    Just res@((x1, y1), (x2, y2)) ->
      let d1 = distance (x0, y0) (x1, y1)
          d2 = distance (x0, y0) (x2, y2)
          wf = whenFail' (putStr (show (x0, y0, r, x)) >> putStr "->" >> putStr (show res) >> putStr " (d1=" >> putStr (show d1) >> putStr ", d2=" >> putStr (show d2) >> print ")")
       in wf $ label "just" $ not isNothing && (d1 `eqEps` d2 && d1 `ltEps` r && x1 `eqEps` x && x2 `eqEps` x)
  where
    result = intersectionCircleVerticalLine (x0, y0) r x

spec_icvl :: Spec
spec_icvl = describe "intersectionCircleVerticalLine" $ do
  it "returns two points when argument is in circle" $
    property
      (\x0 y0 r x -> (r > 0) ==> (abs (x0 - x) <= r) ==> prop_icvl x0 y0 r x False)
  it "returns nothing otherwise" $
    property
      (\x0 y0 r x -> (r > 0) ==> (abs (x0 - x) > r) ==> prop_icvl x0 y0 r x True)

spec_distance :: Spec
spec_distance = describe "distance" $ do
  it "is always positive" $
    property
      (\x0 y0 x1 y1 -> distance (x0, y0) (x1, y1) >= 0)
  it "'triangle' rule?" $
    property
      (\x0 y0 x1 y1 x2 y2 -> distance (x0, y0) (x1, y1) + distance (x1, y1) (x2, y2) >= distance (x0, y0) (x2, y2))
  it "symmetric" $
    property
      (\x0 y0 x1 y1 -> distance (x0, y0) (x1, y1) `eqEps` distance (x1, y1) (x0, y0))

spec :: Spec
spec = do
  spec_icvl
  spec_distance
