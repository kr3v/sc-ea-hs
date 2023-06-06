module Main where

import Control.Lens (over, view)
import Control.Monad.State (execState, runState)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Gloss.Data.Color (white)
import Graphics.Gloss.Data.Picture (translate)
import Graphics.Gloss.Interface.IO.Game (Display (..), Event (EventKey), Key (..), KeyState (..), MouseButton (..), SpecialKey, playIO)
import ScEaHs.GUI.Render (Renderable (..))
import ScEaHs.Game.Surface.Generator (SurfaceWithGenerator, generateSurface, surface, surfaceWithGenerator, updateSurface)
import System.Random (mkStdGen)

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
    (\_ w -> return w)