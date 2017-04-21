module Lib where

import Const(windowBinaryFilePath)
import Types
import Printer(printBox)
import Handle(handle)
import Render(renderGameBox, loadImages)
import Window

import Graphics.Gloss.Interface.Pure.Game

debugOn :: Bool
debugOn = False

run :: IO ()
run = do
    window <- generateWindow windowBinaryFilePath
    images <- loadImages
    if debugOn then dump window else start window images

dump :: Window -> IO ()
dump gw = putStrLn (printBox (game gw))

start :: Window -> Images -> IO ()
start gw imgs = do
    play display bgColor fps gw (renderWindow imgs) handle updateMap
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = blue   -- цвет фона
        fps     = 60      -- кол-во кадров в секунду

updateMap :: Float -> Window -> Window 
updateMap _ gb = gb
