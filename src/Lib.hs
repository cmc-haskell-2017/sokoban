module Lib where

import Printer(printMap)
import GameBox(generateBox)
import Handle(handle)
import Types
import Render

-- import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Interface.Pure.Simulate

gameBoxBinaryFilePath :: String
gameBoxBinaryFilePath = "map.bin"

run :: IO ()
run = do
    images <- loadImages
    start images

start :: Images -> IO()
start imgs = do
    play display bgColor fps (generateBox gameBoxBinaryFilePath) (render imgs) handle updateMap
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

renderMap :: World -> Picture
renderMap gb = (polygon [ (0, 0), (0, -20), (30, -20), (30, 0) ])

updateMap :: Float -> World -> World
updateMap _ gb = gb
