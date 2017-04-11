module Lib where

import Printer(printMap)
import GameBox(generateBox)
import Handle(handle)
import Types

import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate

gameBoxBinaryFilePath :: String
gameBoxBinaryFilePath = "map.bin"

run :: IO ()
run = do
    play display bgColor fps (generateBox gameBoxBinaryFilePath) renderMap handle updateMap
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = white   -- цвет фона
        fps     = 60      -- кол-во кадров в секунду


renderMap :: World -> Picture
renderMap gb = (polygon [ (0, 0), (0, -20), (30, -20), (30, 0) ])

updateMap :: Float -> World -> World
updateMap _ gb = gb
