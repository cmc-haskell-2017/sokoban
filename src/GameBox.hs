module GameBox where

import Types
import System.IO

generateBox :: String -> World
parseBinary :: String -> MapSize -> MapSize -> GameBox
getMap      :: String -> GameMap

generateBox filename = do
    contents <- readFile filename
    let [a, b, c]    = lines contents
    let inputWidth   = read (init a)
    let inputHeight  = read (init b)
    let inputMap     = read (init c)
    return (parseBinary inputMap inputWidth inputHeight)


parseBinary m w h = GameBox
    {
        gameMap = getMap m,
        width   = w,
        height  = h
    }

getMap _ = [WALL, WALL, WALL, WALL, WALL, PERSON, BOX, WALL, WALL, WALL, WALL, WALL]
