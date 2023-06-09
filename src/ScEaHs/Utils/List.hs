module ScEaHs.Utils.List where

import Data.List (uncons)
import Data.Maybe (fromJust)

uncons' :: [a] -> (a, [a])
uncons' = fromJust . uncons
