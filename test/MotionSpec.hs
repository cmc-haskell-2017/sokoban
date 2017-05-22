module MotionSpec (main, spec) where

import Test.Hspec
import GameBox
import Types

main :: IO ()
main = hspec spec

getTestGameBox :: GameBox
getTestGameBox = GameBox {
    gameMap = [
        EMPTY, EMPTY , WALL , BOX  ,
        EMPTY, EMPTY , WALL , EMPTY,
        EMPTY, PERSON, EMPTY, EMPTY
    ],
    width   = 4,
    height  = 3,
    personPos = (1,0),
    oldPersonCell = EMPTY
}

useMovesListToGb :: GameBox -> [Motion] -> GameBox
useMovesListToGb gb xs = foldl (\gba motion -> motionManager motion gba) gb xs
 

spec :: Spec
spec = do
    let gb = (getTestGameBox)
    describe "Sokoban" $ do
        describe "Motion" $ do
            describe "motionManager" $ do
                it "person to LEFT " $ do
                    (getCell (motionManager LEFT gb) (0,0) ) `shouldBe` PERSON
            describe "motionManager" $ do
                it "person to UP " $ do
                    (getCell (motionManager UP gb) (1,1) ) `shouldBe` PERSON
            describe "motionManager" $ do
                it "person to Left by hook " $ do
                    let mList = [UP, LEFT, DOWN]
                    (getCell (useMovesListToGb gb mList) (0,0) ) `shouldBe` PERSON
            describe "motionManager" $ do
                it "person to Wall " $ do
                    let mList = [DOWN, UP, LEFT, RIGHT]
                        gbPTW = GameBox {
                                    gameMap = [       
                                        PERSON
                                    ],
                                    width   = 1,
                                    height  = 1,
                                    personPos = (0,0),
                                    oldPersonCell = EMPTY
                                }
                    (getCell (useMovesListToGb gbPTW mList) (0,0) ) `shouldBe` PERSON