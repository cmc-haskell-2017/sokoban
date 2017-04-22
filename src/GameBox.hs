module GameBox where

import Types

import Debug.Trace

generateBox :: PathToFile -> IO GameBox
parseBinary :: String -> MapSize -> MapSize -> GameBox
getMap      :: String -> GameMap

generateBox filename = do
    contents <- readFile filename
    let [a, b, c]    = lines contents
    let inputWidth   = read a :: Int
    let inputHeight  = read b :: Int
    let inputMap     = read c :: String
    findPersonPosition (parseBinary inputMap inputWidth inputHeight)


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


findPersonPosition :: GameBox -> IO GameBox
findPersonPosition gb = return GameBox {
    gameMap = (gameMap gb),
    width   = (width gb),
    height  = (height gb),
    oldCell = (oldCell gb),
    person  = (find gb PERSON)
}

find :: GameBox -> Cell -> Position
find gb cell = index2pos idx gb
    where
        w    = (width gb)
        h    = (height gb)
        list = (takeWhile (/= cell) (gameMap gb))
        idx  = (length list) + 1


-- | Some Private SHIT:
pos2index :: Position -> GameBox -> Int
pos2index pos gb = (h - y - 1) * w + x + 1
    where
        x = fst pos
        y = snd pos
        w = width gb
        h = height gb

xy2index :: Int -> Int -> GameBox -> Int
xy2index x y gb = (pos2index (x, y) gb)

index2pos :: Int -> GameBox -> Position
index2pos idx gb = ((norm `mod` w), (h - 1 - norm `div` h))
    where
        w = width gb
        h = height gb
        norm = idx - 1


-- | Public methods goes here:
getCell' :: GameBox -> Position -> Cell
getCell' gb pos = getCell gb (fst pos) (snd pos)

getCell :: GameBox -> Int -> Int -> Cell
getCell gb x y
    | x < (width gb) && x >= 0 && y < (height gb) && (y >= 0) = last (take (xy2index x y gb) (gameMap gb))
    | otherwise = WALL


-- | Function `move` called only if Motion is available!
move :: Position -> Position -> GameBox -> GameBox
move to from gb | trace ("move: from=" ++ show from ++ " to=" ++ show to ++ " gb=" ++ show gb) False = undefined
move toPos fromPos gb = 
    GameBox {
        width   = (width gb),
        height  = (height gb),
        oldCell = (getCell' gb toPos),
        person  = toPos,
        gameMap = concat [ (take (from - 1) gm), [old], (take (to - from - 1) (drop from gm)), [PERSON], (drop to gm) ]
    }
    where
        gm      = gameMap gb
        old     = oldCell gb
        fromId  = pos2index fromPos gb
        toId    = pos2index toPos gb
        from    = minimum [fromId, toId]
        to      = maximum [fromId, toId]



-- | Now we start logic:
motionManager :: Motion -> GameBox -> GameBox
motionManager motion gb
    | (motionAvailable motion PERSON moveFrom moveTo gb) == True = (move moveTo moveFrom gb)
    | otherwise = gb
    where
        moveFrom = (person (gb :: GameBox))
        moveTo   = (neighbour motion moveFrom)


motionAvailable :: Motion -> Cell -> Position -> Position -> GameBox -> Bool
motionAvailable motion PERSON from to gb | trace ("motionAvailable: " ++ show motion ++ " from=" ++ show from ++ " to=" ++ show to ++ " " ++ show gb) False = undefined
motionAvailable motion PERSON from to gb 
    | neighbourCell == EMPTY = (flag False) || True
    | neighbourCell == GOAL  = (flag False) || True
    | neighbourCell == WALL  = (flag False) || False
    | neighbourCell == BOX   = (flag False) || (motionAvailable motion BOX to (neighbour motion to) gb)
    | otherwise = (flag False) || False
    where
        neighbourCell = (getCell' gb to)
        flag = trace ("motionAvailable: neighbourPos=" ++ show to ++ " neighbourCell=" ++ show neighbourCell)

motionAvailable motion BOX from to gb
    | neighbourCell == EMPTY = True
    | neighbourCell == GOAL  = True
    | otherwise = False
    where
        neighbourCell = (getCell' gb to)


neighbour :: Motion -> Position -> Position
neighbour LEFT  (x,y) = (x - 1, y)
neighbour RIGHT (x,y) = (x + 1, y)
neighbour UP    (x,y) = (x, y + 1)
neighbour DOWN  (x,y) = (x, y - 1)
neighbour _  pos = pos
