module GameBox where

import Types
import Const

import Debug.Trace

generateBox :: PathToFile -> IO GameBox
parseBinary :: String -> MapSize -> MapSize -> GameBox
getMap      :: String -> GameMap

generateBox filename = do
    contents <- readFile filename
    let fileLines   = lines contents
    let inputWidth  = read (fileLines !! 0) :: Int
    let inputHeight = read (fileLines !! 1) :: Int
    let inputMap    = (concat (drop 2 fileLines))
    return $ findPersonPosition (parseBinary inputMap inputWidth inputHeight)


parseBinary m w h = GameBox {
    gameMap = getMap m,
    width   = w,
    height  = h,
    -- | Just Mock for personPos:
    personPos  = (w, h),
    oldPersonCell = EMPTY
}

getMap str = map convertBinToCell str

convertBinToCell :: Char -> Cell
convertBinToCell '_' = EMPTY
convertBinToCell '#' = WALL
convertBinToCell '@' = BOX
convertBinToCell 'X' = GOAL
convertBinToCell '&' = PERSON
convertBinToCell _   = WALL


findPersonPosition :: GameBox -> GameBox
findPersonPosition gb | trace ("findPersonPosition: " ++ show gb) False = undefined
findPersonPosition gb = GameBox {
    gameMap    = gameMap gb,
    width      = width gb,
    height     = height gb,
    oldPersonCell    = oldPersonCell gb,
    personPos  = find gb PERSON
}

find :: GameBox -> Cell -> Position
find gb cell = index2pos idx gb
    where
        list = takeWhile (/= cell) (gameMap gb)
        idx  = (length list) + 1


-- | Some Private SHIT:
pos2index :: Position -> GameBox -> Int
pos2index pos gb = (h - y - 1) * w + x + 1
    where
        x = fst pos
        y = snd pos
        w = width gb
        h = height gb

index2pos :: Int -> GameBox -> Position
index2pos idx gb = ((norm `mod` w), (h - 1 - norm `div` w))
    where
        w = width gb
        h = height gb
        norm = idx - 1


-- | Public methods goes here:
getCell :: GameBox -> Position -> Cell
getCell gb (x,y)
    | x < (width gb) && x >= 0 && y < (height gb) && (y >= 0) = (gameMap gb) !! idx
    | otherwise = WALL
        where
            idx = pos2index (x,y) gb - 1


-- | Function `move` called only if Motion is available!
move :: Position -> Position -> GameBox -> GameBox
move from to gb | trace ("move: from=" ++ show from ++ " to=" ++ show to ++ " gb=" ++ show gb) False = undefined
move fromPos toPos gb = 
    GameBox {
        width         = width gb,
        height        = height gb,
        oldPersonCell = if objMove == PERSON then (getCell gb toPos) else (oldPersonCell gb),
        personPos     = toPos,
        gameMap       = newGameMap
    }
    where
        gm = gameMap gb
        objMove  = getCell gb fromPos
        nextCell = getCell gb toPos
        newObj
            | nextCell == GOAL && objMove == BOX = GOODBOX
            | nextCell == GOAL && objMove == GOODBOX = GOODBOX
            | objMove == GOODBOX = BOX
            | otherwise = objMove
        oldCell
            | objMove == PERSON  = oldPersonCell gb
            | objMove == BOX     = EMPTY
            | objMove == GOODBOX = GOAL
            | otherwise = EMPTY
        fromId  = pos2index fromPos gb
        toId    = pos2index toPos gb
        left
            | fromId > toId = newObj
            | otherwise = oldCell
        right
            | fromId > toId = oldCell
            | otherwise = newObj
        from    = minimum [fromId, toId]
        to      = maximum [fromId, toId]
        newGameMap = concat [ (take (from - 1) gm), [left], (take (to - from - 1) (drop from gm)), [right], (drop to gm) ]

-- | Now we start logic:
motionManager :: Motion -> GameBox -> GameBox
motionManager motion gb
    | (motionAvailable PERSON motion moveToPos gb) == True = 
        if moveToCell == BOX || moveToCell == GOODBOX then
            (iswinner (move moveFromPos moveToPos moveBoxGB))
        else (move moveFromPos moveToPos gb)
    | otherwise = gb
    where
        moveFromPos    = personPos gb
        moveToPos      = neighbour motion moveFromPos
        moveBoxFromPos = moveToPos
        moveBoxToPos   = neighbour motion moveBoxFromPos
        moveToCell     = getCell gb moveToPos
        moveBoxGB      = move moveBoxFromPos moveBoxToPos gb
        winning        = (\b -> (length (filter (\x -> x == BOX) (gameMap b))) == 0)
        iswinner         = (\x -> if (winning x) then winnerBox else x)


neighbour :: Motion -> Position -> Position
neighbour LEFT  (x,y) = (x - 1, y)
neighbour RIGHT (x,y) = (x + 1, y)
neighbour UP    (x,y) = (x, y + 1)
neighbour DOWN  (x,y) = (x, y - 1)
neighbour _  pos = pos

-- | This function check is current direction available for this object or not?
motionAvailable :: Cell -> Motion -> Position -> GameBox -> Bool
motionAvailable PERSON motion to gb
    | neighbourCell == EMPTY   = True
    | neighbourCell == GOAL    = True
    | neighbourCell == WALL    = False
    | neighbourCell == BOX     = (motionAvailable BOX motion moveToPos gb)
    | neighbourCell == GOODBOX = (motionAvailable GOODBOX motion moveToPos gb)
    | otherwise = False
    where
        neighbourCell = getCell gb to
        moveToPos     = neighbour motion to

motionAvailable object _ to gb
    | object == BOX || object == GOODBOX =
        if neighbourCell == EMPTY || neighbourCell == GOAL then True else False
    | otherwise = False
    where
        neighbourCell = getCell gb to

