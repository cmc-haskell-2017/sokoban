module WinGameSpec (main, spec) where

import Test.Hspec
import GameBox
import Types
import Const

main :: IO ()
main = hspec spec

getTGB1 :: GameBox
getTGB1 = GameBox {
    gameMap = [
        PERSON, EMPTY, EMPTY, EMPTY, EMPTY, EMPTY,
        WALL, BOX, WALL, BOX, WALL, BOX,
        WALL, GOAL, WALL, GOAL, WALL, GOAL
    ],
    width   = 6,
    height  = 3,
    personPos = (0,2),
    oldPersonCell = EMPTY
}

getTGB2 :: GameBox
getTGB2 = GameBox {
    gameMap = [
        WALL, WALL, GOAL, WALL, WALL,
        WALL, WALL, BOX, WALL, WALL,
        GOAL, BOX, PERSON, BOX, GOAL,
        WALL, WALL, BOX, WALL, WALL,
        WALL, WALL, GOAL, WALL, WALL
    ],
    width   = 5,
    height  = 5,
    personPos = (2,2),
    oldPersonCell = EMPTY
}

getTGB3 :: GameBox
getTGB3 = GameBox {
    gameMap = [
        GOAL, PERSON, BOX, GOAL
    ],
    width   = 4,
    height  = 1,
    personPos = (1,0),
    oldPersonCell = EMPTY
}



useMovesListToGb :: GameBox -> [Motion] -> GameBox
useMovesListToGb gb xs = foldl (\gba motion -> motionManager motion gba) gb xs


spec :: Spec
spec = do
    describe "Sokoban" $ do
        describe "EndGame" $ do
            describe "gb1" $ do
                it "person to win1" $ do
                    let mList = [RIGHT, DOWN, UP, RIGHT, RIGHT, DOWN, RIGHT, RIGHT, DOWN, UP]
                        gb = (getTGB1)
                    gameMap (useMovesListToGb gb mList) `shouldBe`  gameMap winnerBox
            describe "gb2" $ do
                it "person to win2" $ do
                    let mList = [UP, DOWN, RIGHT, LEFT, DOWN, UP, LEFT, RIGHT]
                        gb = (getTGB2)
                    gameMap winnerBox == gameMap (useMovesListToGb gb mList) `shouldBe` True
            describe "gb3" $ do
                it "person to win3" $ do
                    let mList = [RIGHT, UP, RIGHT, LEFT]
                        gb = (getTGB3)
                    gameMap winnerBox == gameMap (useMovesListToGb gb mList) `shouldBe` True

