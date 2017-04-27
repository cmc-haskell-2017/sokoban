module Types where

import Graphics.Gloss.Interface.Pure.Game

type PathToFile = String

type MapSize  = Int
type Position = (Int,Int)

data Cell    = EMPTY | WALL | BOX | GOAL | PERSON
type GameMap = [Cell]
data GameBox = GameBox {
    gameMap :: GameMap,
    width   :: MapSize,
    height  :: MapSize,
    person  :: Position,
    oldCell :: Cell
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

instance Show Cell where
    show WALL      = "WALL"
    show EMPTY     = "EMPTY"
    show BOX       = "BOX"
    show GOAL      = "GOAL"
    show PERSON    = "PERSON"

instance Show Motion where
    show LEFT   = "LEFT"
    show RIGHT  = "RIGHT"
    show UP     = "UP"
    show DOWN   = "DOWN"
    show MENU   = "MENU"

instance Show GameBox where
    show gb = "GB:{ " ++ show w ++ "x" ++ show h ++ " person=" ++ show personPos ++ " old=" ++ show old ++ " gm=" ++ showList gm " }"
        where
            w = width gb
            h = height gb
            gm = gameMap gb
            old = oldCell gb
            personPos = (person (gb :: GameBox))



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
data Interface = Interface {
    buttons :: [Button]
}

type Action = (Interface -> Interface)

type Button = (String, Action, Selected)

type Selected = Bool

data Motion = LEFT | RIGHT | UP | DOWN | MENU


data WindowTag = GAME | INTERFACE

instance Eq WindowTag where
    (==) GAME GAME              = True
    (==) INTERFACE INTERFACE    = True
    (==) _ _                    = False

data Window = Window {
    tag  :: WindowTag,
    game :: GameBox,
    ui   :: Interface
}
