module ScEaHs.Utils.Geometry (intersectionCircleVerticalLine, distance) where

import Graphics.Gloss (Point)

intersectionCircleVerticalLine ::
  Point -> -- center of the circle
  Float -> -- radius of the circle
  Float -> -- x coordinate of vertical line
  Maybe (Point, Point) -- intersection points
intersectionCircleVerticalLine (x0, y0) r x
  | discriminant < 0 = Nothing
  | otherwise = Just ((x, y1), (x, y2))
  where
    discriminant = r ^ 2 - (x - x0) ^ 2
    y1 = y0 - sqrt discriminant
    y2 = y0 + sqrt discriminant

distance :: Point -> Point -> Float
distance (x0, y0) (x, y) = sqrt $ (x - x0) ^ 2 + (y - y0) ^ 2
