module Lib where

import Const(gameBoxBinaryFilePath)
import Types
import Printer(printMap)
import GameBox(generateBox)
import Handle(handle)

import Graphics.Gloss.Interface.Pure.Game

debugOn :: Bool
debugOn = True

run :: IO ()
run = do
    box <- generateBox gameBoxBinaryFilePath
    if debugOn then dump box else start box


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
