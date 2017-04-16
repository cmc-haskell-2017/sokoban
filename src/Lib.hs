module Lib where

import Printer(printMap)
import GameBox(generateBox)
import Handle(handle)
import Types
import Const(gameBoxBinaryFilePath)

import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate

run :: IO ()
run = do
    box <- generateBox gameBoxBinaryFilePath
    dump box

dump :: GameBox -> IO ()
dump gb = putStrLn (printMap gb)


start :: GameBox -> IO ()
start gb = do
    play display bgColor fps gb renderMap handle updateMap
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = white   -- цвет фона
        fps     = 60      -- кол-во кадров в секунду


renderMap :: GameBox -> Picture
renderMap gb = (polygon [ (0, 0), (0, -20), (30, -20), (30, 0) ])

updateMap :: Float -> GameBox -> GameBox
updateMap _ gb = gb
