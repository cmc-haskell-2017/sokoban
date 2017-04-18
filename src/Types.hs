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

instance Show Cell where
    show WALL      = "WALL"
    show EMPTY     = "EMPTY"
    show BOX       = "BOX"
    show GOAL      = "GOAL"
    show PERSON    = "PERSON"

instance Eq Cell where
    (==) EMPTY EMPTY   = True
    (==) WALL WALL     = True
    (==) BOX BOX       = True
    (==) GOAL GOAL     = True
    (==) PERSON PERSON = True
    (==) EMPTY _       = False
    (==) WALL _        = False
    (==) BOX _         = False
    (==) GOAL _        = False
    (==) PERSON _      = False
