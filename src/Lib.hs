module Lib where

import Const(gameBoxBinaryFilePath)
import Types
import Printer(printMap)
import GameBox(generateBox)
import Handle(handle)
import Render(render, loadImages)

import Graphics.Gloss.Interface.Pure.Game

debugOn :: Bool
debugOn = False

run :: IO ()
-- run = putStrLn (printMap (generateMap))
run = do
    box <- generateBox gameBoxBinaryFilePath
    images <- loadImages
    if debugOn then dump box else start box images

dump :: GameBox -> IO ()
dump gb = putStrLn (printMap gb)

start :: GameBox -> Images -> IO ()
start gb imgs = do
    play display bgColor fps gb (render imgs) handle updateMap
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = blue   -- цвет фона
        fps     = 60      -- кол-во кадров в секунду

updateMap :: Float -> GameBox -> GameBox
updateMap _ gb = gb
