module ScEaHs.GUI.Render.Symbols (pictures, pictures') where

import Control.Monad (liftM2)
import Data.List (subsequences)
import Graphics.Gloss (Picture (..), circle, line, translate)

historySymbols :: [Picture]
historySymbols =
  [ translate (-0.4) (-0.4) $ circle 4,
    line [(-5, -5), (5, 5)] <> line [(-5, 5), (5, -5)], -- x
    line [(-5, 0), (5, 0)] <> line [(0, -5), (0, 5)], -- +
    line [(-5, -5), (-5, 5)] <> line [(5, -5), (5, 5)] <> line [(-5, -5), (5, -5)] <> line [(-5, 5), (5, 5)], -- square
    line [(-5, 0), (0, 5)] <> line [(0, 5), (5, 0)] <> line [(5, 0), (0, -5)] <> line [(0, -5), (-5, 0)], -- diamond
    line [(-5, -5), (5, -5)] <> line [(5, -5), (0, 5)] <> line [(0, 5), (-5, -5)] -- triangle
  ]

pictures' :: [Picture]
pictures' = Pictures <$> filter (liftM2 (&&) (> 0) (<= 2) . length) (subsequences historySymbols)

pictures :: [Picture]
pictures = cycle pictures'