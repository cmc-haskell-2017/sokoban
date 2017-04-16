module Lib where

-- import Printer(printMap)
import Types
import Render

-- import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Interface.Pure.Simulate


run :: IO ()
-- run = putStrLn (printMap (generateMap))
run = do
    images <- loadImages
    start images

start :: Images -> IO()
start imgs = do
    play display bgColor fps (generateMap) (render imgs) handle updateMap
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = white   -- цвет фона
        fps     = 60      -- кол-во кадров в секунду


generateMap :: GameBox
generateMap = GameBox
    {
        gameMap = [WALL, WALL, WALL, WALL, WALL, PERSON, BOX, WALL, WALL, WALL, WALL, WALL],
        width   = 4,
        height  = 1
    }

renderMap :: GameBox -> Picture
renderMap _ = (polygon [ (0, 0), (0, -20), (30, -20), (30, 0) ])

updateMap :: Float -> GameBox -> GameBox
updateMap _ gb = gb

leftDown  :: GameBox -> GameBox
leftDown  gb = gb
rightDown :: GameBox -> GameBox
rightDown gb = gb
leftUp    :: GameBox -> GameBox
leftUp    gb = gb
rightUp   :: GameBox -> GameBox
rightUp   gb = gb

handle :: Event -> GameBox -> GameBox
handle (EventKey (SpecialKey KeyLeft) Down _ _) u   = leftDown u
handle (EventKey (SpecialKey KeyRight) Down _ _) u  = rightDown u
handle (EventKey (SpecialKey KeyLeft) Up _ _) u     = leftUp u
handle (EventKey (SpecialKey KeyRight) Up _ _) u    = rightUp u
handle _ u = u
