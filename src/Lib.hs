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
_printMap     :: GameMap -> MapSize -> Int -> String

run :: IO ()
run = putStrLn (printMap generateMap)

printMap gb = _printMap (gameMap gb) (width gb) 0

_printMap [] _ _ = ""
_printMap (x:xs) w i = ( endLineFilter x True ) + (_printMap xs w i + 1)


endLineFilter cell True  = (printCell cell) + "\n"
endLineFilter cell False = (printCell cell)


printCell EMPTY   = " "
printCell WALL    = "▩"
printCell BOX     = "□"
printCell GOAL    = "◯"
printCell PERSON  = "&"


generateMap :: GameBox
generateMap = GameBox
    {
        gameMap = [ EMPTY ],
        width   = 1,
        height  = 1
    }
