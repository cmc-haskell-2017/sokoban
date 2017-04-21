module Types where

import Graphics.Gloss.Interface.Pure.Game

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
screenWidth  = 900

screenHeight :: Int
screenHeight = 700

screenLeft   :: Int
screenLeft   = 200

screenTop    :: Int
screenTop    = 200

data Movement = MoveLeft | MoveRight | MoveUp | MoveDown

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

-- здесь мог бы быть ваш интерфейс с пользователем
type Interface = Int

data Motion = LEFT | RIGHT | UP | DOWN | MENU

data WindowTag = GAME | INTERFACE

instance Eq WindowTag where
    (==) GAME GAME              = True
    (==) INTERFACE INTERFACE    = True
    (==) _ _                    = False

data Window = Window {
    tag :: WindowTag,
    game :: GameBox,
    ui :: Interface
}
