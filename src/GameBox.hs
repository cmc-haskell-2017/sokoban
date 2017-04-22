module GameBox where

import Types

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
    -- | Just Mock for person:
    person  = (w, h),
    oldCell = EMPTY
}

getMap str = map convertBinToCell str

convertBinToCell :: Char -> Cell
convertBinToCell ' ' = EMPTY
convertBinToCell '#' = WALL
convertBinToCell '@' = BOX
convertBinToCell 'X' = GOAL
convertBinToCell '&' = PERSON
convertBinToCell _   = WALL


findPersonPosition :: GameBox -> GameBox
findPersonPosition gb | trace ("findPersonPosition: " ++ show gb) False = undefined
findPersonPosition gb = GameBox {
    gameMap = gameMap gb,
    width   = width gb,
    height  = height gb,
    oldCell = oldCell gb,
    person  = find gb PERSON
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
getCell gb pos
    | x < (width gb) && x >= 0 && y < (height gb) && (y >= 0) = last (take (pos2index pos gb) (gameMap gb))
    | otherwise = WALL
        where
            x = (fst pos)
            y = (snd pos)


-- | Function `move` called only if Motion is available!
move :: Position -> Position -> GameBox -> GameBox
move from to gb | trace ("move: from=" ++ show from ++ " to=" ++ show to ++ " gb=" ++ show gb) False = undefined
move fromPos toPos gb = 
    GameBox {
        width   = width gb,
        height  = height gb,
        oldCell = getCell gb toPos,
        person  = toPos,
        gameMap = concat [ (take (from - 1) gm), [left], (take (to - from - 1) (drop from gm)), [right], (drop to gm) ]
    }
    where
        gm      = gameMap gb
        old     = oldCell gb
        fromId  = pos2index fromPos gb
        toId    = pos2index toPos gb
        from    = minimum [fromId, toId]
        to      = maximum [fromId, toId]
        left
            | fromId > toId = PERSON
            | otherwise = old
        right
            | fromId > toId = old
            | otherwise = PERSON

-- | Now we start logic:
motionManager :: Motion -> GameBox -> GameBox
motionManager motion gb
    | (motionAvailable PERSON motion moveTo gb) == True = (move moveFrom moveTo gb)
    | otherwise = gb
    where
        moveFrom = (person (gb :: GameBox))
        moveTo   = (neighbour motion moveFrom)

neighbour :: Motion -> Position -> Position
neighbour LEFT  (x,y) = (x - 1, y)
neighbour RIGHT (x,y) = (x + 1, y)
neighbour UP    (x,y) = (x, y + 1)
neighbour DOWN  (x,y) = (x, y - 1)
neighbour _  pos = pos

-- | This function check is current direction available for this object or not?
motionAvailable :: Cell -> Motion -> Position -> GameBox -> Bool
motionAvailable PERSON motion to gb
    | neighbourCell == EMPTY = True
    | neighbourCell == GOAL  = True
    | neighbourCell == WALL  = False
    | neighbourCell == BOX   = (motionAvailable BOX motion (neighbour motion to) gb)
    | otherwise = False
    where
        neighbourCell = getCell gb to

motionAvailable BOX _ to gb
    | neighbourCell == EMPTY = True
    | neighbourCell == GOAL  = True
    | otherwise = False
    where
        neighbourCell = getCell gb to

motionAvailable _ _ _ _ = False
