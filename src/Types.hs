module Types where

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

screenWidth  :: Int
screenWidth  = 450

screenHeight :: Int
screenHeight = 700

screenLeft   :: Int
screenLeft   = 200

screenTop    :: Int
screenTop    = 200
