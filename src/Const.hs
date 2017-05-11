module Const where

import Types

gameBoxBinaryFilePath :: PathToFile
gameBoxBinaryFilePath = "data/map.bin"

windowBinaryFilePath :: PathToFile
windowBinaryFilePath = "data/map.bin"

winnerBox :: GameBox
winnerBox = GameBox {
    gameMap       = [GOODBOX, BOX, GOODBOX, BOX, GOODBOX, BOX, GOODBOX, BOX, GOODBOX],
    width         = 3,
    height        = 3,
    personPos     = (1, 1),
    oldPersonCell = EMPTY
    }
