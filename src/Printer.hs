module Printer where

import Types

printCell     :: Cell -> String
printMap      :: GameBox -> String
insertElem    :: Int -> a -> [a] -> [a]

insertElem _ _  [] = []
insertElem i e x = (take i x) ++ [e] ++ insertElem i e (drop i x)

run :: IO ()
run = putStrLn (printMap (generateMap))


printMap gb = foldl (++) "" (insertElem (width gb) "\n" (map printCell (gameMap gb)))


printCell EMPTY   = " "
printCell WALL    = "%"
printCell BOX     = "@"
printCell GOAL    = "X"
printCell PERSON  = "&"


generateMap :: GameBox
generateMap = GameBox
    {
        gameMap = [WALL, WALL, WALL, WALL, WALL, PERSON, BOX, WALL, WALL, WALL, WALL, WALL],
        width   = 4,
        height  = 1
    }
