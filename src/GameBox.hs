module GameBox where

import Types
import System.IO

generateBox :: String -> IO GameBox
parseBinary :: String -> MapSize -> MapSize -> IO GameBox
getMap      :: String -> GameMap

generateBox filename = do
    contents <- readFile filename
    let [a, b, c]    = lines contents
    let inputWidth   = read a :: Int
    let inputHeight  = read b :: Int
    let inputMap     = read c :: String
    parseBinary inputMap inputWidth inputHeight


parseBinary m w h = return GameBox {
        gameMap = getMap m,
        width   = w,
        height  = h
    }

getMap _ = [WALL, WALL, WALL, WALL, WALL, PERSON, BOX, WALL, WALL, WALL, WALL, WALL]

-- | Public methods goes here:
getCell :: GameBox -> Int -> Int -> Cell
getCell gb x y = last (take ((x * width gb) + y) (gameMap gb))
