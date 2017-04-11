module Lib where

import Printer(printMap)
import Types

run :: IO ()
run = putStrLn (printMap (generateMap))


generateMap :: GameBox
generateMap = GameBox
    {
        gameMap = [WALL, WALL, WALL, WALL, WALL, PERSON, BOX, WALL, WALL, WALL, WALL, WALL],
        width   = 4,
        height  = 1
    }
