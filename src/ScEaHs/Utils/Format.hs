module ScEaHs.Utils.Format where

import Numeric (showFFloat)

showF2 :: RealFloat a => a -> String
showF2 = flip (showFFloat (Just 2)) ""

showTF2 :: RealFloat a => (a, a) -> String
showTF2 (a, b) = "(" ++ showF2 a ++ "," ++ showF2 b ++ ")"
