module Editor where

import Types
import Printer(printBox)
import Render(renderGameBox, loadImages)
import Window

import GameBox
import Interface
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Debug.Trace
import Const

runEditor :: IO ()
runEditor = do
    let userSizeGB = GameBox {
            gameMap = [EMPTY],
            width = 1,
            height = 1,
            personPos = (0,1),
            oldPersonCell = EMPTY
        }
        window = Window {
            tag = GAME,
            game = userSizeGB,
            ui = 7,
            savedMap = []
        }
    startEditor window

changeSize :: Motion -> Window -> Window
changeSize m gw = if gameMap gb /= fillWithEmpty h w then gw
    else 
        Window {
            savedMap = savedMap gw,
            tag = tag gw,
            ui = ui gw,
            game = GameBox {
                width = newW,
                height = newH,
                oldPersonCell = EMPTY,
                personPos = (0, newH-1),
                gameMap = fillWithEmpty newH newW
            }
        }
        where
            gb = game gw
            h = height gb
            w = width gb
            newW  
                | m == LEFT =
                    if w > 1 then w-1 else w
                | m == RIGHT =
                    if w < 12  then w+1 else w
                | otherwise = w
            newH
                | m == UP =
                    if h > 1 then h-1 else h
                | m == DOWN =
                    if h < 12 then h+1 else h
                | otherwise = h

fillWithEmpty :: Int -> Int -> GameMap
fillWithEmpty h w = map (\x -> EMPTY) [1..h*w]

startEditor :: Window -> IO ()
startEditor gw = do
    images <- loadImages
    playIO display bgColor fps gw (renderWindowIO images) handleMoves updateMapEditor
    where
        display = InWindow "Sokoban" (screenWidth, screenHeight) (screenLeft, screenTop)
        bgColor = blue
        fps     = 60

renderWindowIO :: Images -> Window -> IO Picture
renderWindowIO images window = pure(renderGameBox images (game window))

updateMapEditor :: Float -> Window -> IO Window 
updateMapEditor _ gb = pure(gb)

handleMoves :: Event -> Window -> IO Window
handleMoves (EventKey (SpecialKey KeyLeft) Down _ _) window   = pure(motionManagerEdit LEFT window)
handleMoves (EventKey (SpecialKey KeyRight) Down _ _) window  = pure(motionManagerEdit RIGHT window)
handleMoves (EventKey (SpecialKey KeyUp) Down _ _) window     = pure(motionManagerEdit UP window)
handleMoves (EventKey (SpecialKey KeyDown) Down _ _) window   = pure(motionManagerEdit DOWN window)
handleMoves (EventKey (Char 'p') Down _ _) window  = pure(setEditor PERSON window)
handleMoves (EventKey (Char 'g') Down _ _) window  = pure(setEditor GOAL window)
handleMoves (EventKey (Char 'b') Down _ _) window  = pure(setEditor BOX window)
handleMoves (EventKey (Char 'e') Down _ _) window  = pure(setEditor EMPTY window)
handleMoves (EventKey (Char 'o') Down _ _) window  = pure(setEditor WALL window)

handleMoves (EventKey (Char 'a') Down _ _) window  = pure(changeSize LEFT window)
handleMoves (EventKey (Char 'd') Down _ _) window  = pure(changeSize RIGHT window)
handleMoves (EventKey (Char 'w') Down _ _) window  = pure(changeSize UP window)
handleMoves (EventKey (Char 's') Down _ _) window  = pure(changeSize DOWN window)
handleMoves (EventKey (Char 'c') Down _ _) window  = pure(cleanMap window)

handleMoves (EventKey (Char 'n') Down _ _) window = saveNewMap window

handleMoves _ window = pure(window)
    
saveNewMap :: Window -> IO Window
saveNewMap gw = do
    writeFile savedMapsBinaryFilePath (show w ++ ['\n'] ++ show h ++ ['\n'] ++ printBox gb)
    pure(Window {
        tag = tag gw,
        ui = ui gw,
        game = GameBox {
            width = 1,
            height = 1,
            oldPersonCell = EMPTY,
            personPos = (0,1),
            gameMap = [EMPTY]
        }
    })
    where
        gb = game gw
        w = width gb
        h = height gb

setEditor :: Cell -> Window -> Window
setEditor c gw = 
    Window {
        savedMap = savedMap gw,
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



cleanMap :: Window -> Window
cleanMap gw = Window {
            savedMap = savedMap gw,
            tag = tag gw,
            ui = ui gw,
            game = GameBox {
                width = w,
                height = h,
                oldPersonCell = EMPTY,
                personPos = (0, h-1),
                gameMap = fillWithEmpty h w
            }
        }
    where 
        gb = game gw
        h = height gb
        w = width gb

motionManagerEdit :: Motion -> Window -> Window
motionManagerEdit motion window = Window {
        tag = tag window,
        game = motionManagerGB motion (game window),
        ui = ui window,
        savedMap = savedMap window
    }

motionManagerGB :: Motion -> GameBox -> GameBox
motionManagerGB motion gb
    | (motionAvailEditor moveToPos gb) == True = 
        moveEditor moveFromPos moveToPos gb
    | otherwise = gb
    where
        moveFromPos    = personPos gb
        moveToPos      = neighbour motion moveFromPos

motionAvailEditor :: Position -> GameBox -> Bool
motionAvailEditor (w,h) gb
    | w >= (width gb) || h >= (height gb) || h < 0 || w < 0 = False
    | otherwise = True

moveEditor :: Position -> Position -> GameBox -> GameBox
moveEditor _ _ gb | trace ("MOVE oldPersonCell: " ++ show (oldPersonCell gb)) False = undefined
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