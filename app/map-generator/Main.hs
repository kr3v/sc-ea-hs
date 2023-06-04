module Main where
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (mkStdGen)
import Graphics.Gloss.Data.Picture (translate)
import ScEaHs.Game.Surface.Generator (surfaceWithGenerator, generateSurface, surface, SurfaceWithGenerator, updateSurface)
import Control.Monad.State (runState, execState)
import Control.Lens (view, over)
import Graphics.Gloss.Interface.IO.Game (playIO, Display (..), SpecialKey, Event (EventKey), Key (..), MouseButton (..), KeyState (..))
import Graphics.Gloss.Data.Color (white)
import ScEaHs.GUI.Render (Renderable(..))

eventHandler :: Event -> SurfaceWithGenerator -> IO SurfaceWithGenerator
eventHandler (EventKey (MouseButton LeftButton) Up _ _) s = return $ execState updateSurface s
eventHandler e s = print e >> return s

main :: IO ()
main = do
  w <- getPOSIXTime
  let g = mkStdGen $ round w

  let mx = 1000 :: Int
      my = 1000 :: Int
      windowSize = (mx, my)
      transformer = translate (-fromIntegral mx / 2) (-fromIntegral my / 2)
      sfg = surfaceWithGenerator g (runState $ generateSurface mx my)
      sf = view surface sfg

  playIO
    (InWindow "GameEvent" windowSize (10, 10))
    white
    60
    sfg
    (return . transformer . render . view surface)
    eventHandler
    (\ _ w -> return w)