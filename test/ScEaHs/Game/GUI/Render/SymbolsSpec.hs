module ScEaHs.Game.GUI.Render.SymbolsSpec where

import ScEaHs.GUI.Render.Symbols (pictures, pictures')
import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Testable (..), (==>))

spec :: Spec
spec = do
  describe "historyPictures" $ do
    it "should be a list of pictures" $
      property (\n -> (n > 0 && n <= 100 * length pictures') ==> n == length (take n pictures))