module Types where

-- import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Interface.Pure.Simulate

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

-- Types and data for rendering

data Images = Images
    {
        box :: Picture,
        person :: Picture,
        wall :: Picture,
        empty :: Picture,
        mark :: Picture
    }

screenWidth  :: Int
screenWidth  = 450

screenHeight :: Int
screenHeight = 700

screenLeft   :: Int
screenLeft   = 200

screenTop    :: Int
screenTop    = 200
