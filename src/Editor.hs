module Editor where

import Const(windowBinaryFilePath)
import Types
import Printer(printBox)
import Handle(handle)
import Render(renderGameBox, loadImages)
import Window

import GameBox
import Interface
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

runEditor :: Int -> Int -> IO ()
runEditor h w = do 
    let userSizeGB = GameBox {
            gameMap = fillWithEmpty h w,
            width = w,
            height = h,
            personPos = (0,h-1),
            oldPersonCell = EMPTY
        }
        window = Window {
            tag = GAME,
            game = userSizeGB,
            ui = 7
        }
    startEditor window

fillWithEmpty :: Int -> Int -> GameMap
--fillWithEmpty h w = [
--                    EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
--                    EMPTY, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
--                    EMPTY, EMPTY, BOX, EMPTY, BOX, EMPTY,
--                    EMPTY, GOAL, BOX, EMPTY, BOX, EMPTY,
--                    ]

fillWithEmpty h w = map (\x -> EMPTY) [1..h*w]

startEditor :: Window -> IO ()
startEditor gw = do
    images <- loadImages
    play display bgColor fps gw (renderWindow images) handleMoves updateMapEditor
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = blue   -- цвет фона
        fps     = 60     -- кол-во кадров в секунду

startMoves :: Window -> IO ()
startMoves gw = do
    images <- loadImages
    play display bgColor fps gw (renderWindow images) handleMoves updateMapEditor
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = blue   -- цвет фона
        fps     = 60     -- кол-во кадров в секунду

updateMapEditor :: Float -> Window -> Window 
updateMapEditor _ gb = gb


handleMoves :: Event -> Window -> Window
handleMoves (EventKey (SpecialKey KeyLeft) Down _ _) window   = motionManagerEdit LEFT window
handleMoves (EventKey (SpecialKey KeyRight) Down _ _) window  = motionManagerEdit RIGHT window
handleMoves (EventKey (SpecialKey KeyUp) Down _ _) window     = motionManagerEdit UP window
handleMoves (EventKey (SpecialKey KeyDown) Down _ _) window   = motionManagerEdit DOWN window
handleMoves (EventKey (Char 'p') Down _ _) window  = setEditor PERSON window
handleMoves (EventKey (Char 'g') Down _ _) window  = setEditor GOAL window
handleMoves (EventKey (Char 'b') Down _ _) window  = setEditor BOX window
handleMoves (EventKey (Char 'e') Down _ _) window  = setEditor EMPTY window
handleMoves (EventKey (Char 'w') Down _ _) window  = setEditor WALL window
handleMoves (EventKey (Char 'o') Down _ _) window  = setEditor GOODBOX window
--handleMoves (EventKey (Char 'c') Down _ _) window = startEditor window 
handleMoves _ window = window

handleChoose :: Event -> Window -> Window
handleChoose _ window = window

setEditor :: Cell -> Window -> Window
setEditor c gw = 
    Window {
        tag = tag gw,
        ui = ui gw,
        game = GameBox {
            width = w,
            height = h,
            oldPersonCell = getCell gb (0, h-1),
            personPos = (0, h-1),
            gameMap = c : drop 1 (gameMap gb)
        }
    }
    where
        gb = game gw
        h = height gb
        w = width gb


motionManagerEdit :: Motion -> Window -> Window
motionManagerEdit motion window 
    | tag window == GAME = Window {
        tag = tag window,
        game = motionManagerGB motion (game window),
        ui = ui window
    }

motionManagerGB :: Motion -> GameBox -> GameBox
motionManagerGB motion gb
    | (motionAvailEditor motion moveToPos gb) == True = 
        moveEditor moveFromPos moveToPos gb
    | otherwise = gb
    where
        moveFromPos    = personPos gb
        moveToPos      = neighbour motion moveFromPos
        moveBoxFromPos = moveToPos
        moveBoxToPos   = neighbour motion moveBoxFromPos
        moveToCell     = getCell gb moveToPos
        moveBoxGB      = moveEditor moveBoxFromPos moveBoxToPos gb

motionAvailEditor :: Motion -> Position -> GameBox -> Bool
motionAvailEditor motion (w,h) gb
    | w >= (width gb) || h >= (height gb) || h < 0 || w < 0 = False
    | otherwise = True

moveEditor :: Position -> Position -> GameBox -> GameBox
moveEditor from to gb | trace ("MOVE oldPersonCell: " ++ show (oldPersonCell gb)) False = undefined
moveEditor fromPos toPos gb = 
    GameBox {
        width         = width gb,
        height        = height gb,
        oldPersonCell = getCell gb toPos,
        personPos     = toPos,
        gameMap       = newGameMap
    }
    where
        gm = gameMap gb
        objMove  = getCell gb fromPos
        nextCell = getCell gb toPos

        fromId  = pos2index fromPos gb
        toId    = pos2index toPos gb
        left
            | fromId > toId = objMove
            | otherwise = oldPersonCell gb
        right
            | fromId > toId = oldPersonCell gb
            | otherwise = objMove
        from    = minimum [fromId, toId]
        to      = maximum [fromId, toId]
        newGameMap = concat [ (take (from - 1) gm), [left], 
                              (take (to - from - 1) (drop from gm)), 
                              [right], (drop to gm) ]