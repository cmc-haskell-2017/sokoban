module Lib where

type MapSize  = Int
type Position = (Int,Int)

data Cell    = EMPTY | WALL | BOX | GOAL | PERSON
type GameMap = [Cell]
data GameBox = GameBox {
    gameMap :: GameMap,
    width   :: MapSize,
    height  :: MapSize
-- | person :: Position,
}

printCell     :: Cell -> String
endLineFilter :: Cell -> Bool -> String
printMap      :: GameBox -> String
insertElem    :: Int -> a -> [a] -> [a]

insertElem _ _  [] = []
insertElem i e x = (take i x) ++ [e] ++ insertElem i e (drop i x)

run :: IO ()
run = putStrLn (printMap (generateMap))


printMap gb = foldl (++) "" (insertElem (width gb) "\n" (map printCell (gameMap gb)))


endLineFilter cell True  = (printCell cell)
endLineFilter cell False = (printCell cell)


printCell EMPTY   = " "
printCell WALL    = "#"
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
