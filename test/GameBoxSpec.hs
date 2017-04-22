module GameBoxSpec (main, spec) where

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
    height  = 3
}

spec :: Spec
spec = do
    let gb = (getTestGameBox)
    describe "Sokoban" $ do
        describe "GameBox" $ do
            describe "getCell" $ do
                it "TOP RIGHT" $ do
                    (getCell gb (5,5)) `shouldBe` WALL
            describe "getCell" $ do
                it "CENTER LEFT"  $ do
                    (getCell gb (0,1)) `shouldBe` EMPTY
            describe "getCell" $ do
                it "CENTER CENTER"  $ do
                    (getCell gb (2,2)) `shouldBe` WALL
            describe "getCell" $ do
                it "CENTER RIGHT"  $ do
                    (getCell gb (3,1)) `shouldBe` EMPTY
            describe "getCell" $ do
                it "DOWN LEFT"  $ do
                    (getCell gb (0,0)) `shouldBe` EMPTY

            describe "pos2index" $ do
                it "(3,0) -> 12" $ do
                    (pos2index (3,0) gb) `shouldBe` 12
            describe "pos2index" $ do
                it "(2,0) -> 11" $ do
                    (pos2index (2,0) gb) `shouldBe` 11
            describe "pos2index" $ do
                it "(1,0) -> 10" $ do
                    (pos2index (1,0) gb) `shouldBe` 10
            describe "pos2index" $ do
                it "(0,0) -> 9" $ do
                    (pos2index (0,0) gb) `shouldBe` 9
            describe "pos2index" $ do
                it "(3,1) -> 8" $ do
                    (pos2index (3,1) gb) `shouldBe` 8
            describe "pos2index" $ do
                it "(2,1) -> 7" $ do
                    (pos2index (2,1) gb) `shouldBe` 7
            describe "pos2index" $ do
                it "(1,1) -> 6" $ do
                    (pos2index (1,1) gb) `shouldBe` 6
            describe "pos2index" $ do
                it "(0,1) -> 5" $ do
                    (pos2index (0,1) gb) `shouldBe` 5
            describe "pos2index" $ do
                it "(3,2) -> 4" $ do
                    (pos2index (3,2) gb) `shouldBe` 4
            describe "pos2index" $ do
                it "(2,2) -> 3" $ do
                    (pos2index (2,2) gb) `shouldBe` 3
            describe "pos2index" $ do
                it "(1,2) -> 2" $ do
                    (pos2index (1,2) gb) `shouldBe` 2
            describe "pos2index" $ do
                it "(0,2) -> 1" $ do
                    (pos2index (0,2) gb) `shouldBe` 1

            describe "index2pos" $ do
                it "12 -> (3,0)" $ do
                    (index2pos 12 gb) `shouldBe` (3,0)
            describe "index2pos" $ do
                it "11 -> (2,0)" $ do
                    (index2pos 11 gb) `shouldBe` (2,0)
            describe "index2pos" $ do
                it "10 -> (1,0)" $ do
                    (index2pos 10 gb) `shouldBe` (1,0)
            describe "index2pos" $ do
                it "9 -> (0,0)" $ do
                    (index2pos 9 gb) `shouldBe` (0,0)
            describe "index2pos" $ do
                it "8 -> (3,1)" $ do
                    (index2pos 8 gb) `shouldBe` (3,1)
            describe "index2pos" $ do
                it "7 -> (2,1)" $ do
                    (index2pos 7 gb) `shouldBe` (2,1)
            describe "index2pos" $ do
                it "6 -> (1,1)" $ do
                    (index2pos 6 gb) `shouldBe` (1,1)
            describe "index2pos" $ do
                it "5 -> (0,1)" $ do
                    (index2pos 5 gb) `shouldBe` (0,1)
            describe "index2pos" $ do
                it "4 -> (3,2)" $ do
                    (index2pos 4 gb) `shouldBe` (3,2)
            describe "index2pos" $ do
                it "3 -> (2,2)" $ do
                    (index2pos 3 gb) `shouldBe` (2,2)
            describe "index2pos" $ do
                it "2 -> (1,2)" $ do
                    (index2pos 2 gb) `shouldBe` (1,2)
            describe "index2pos" $ do
                it "1 -> (0,2)" $ do
                    (index2pos 1 gb) `shouldBe` (0,2)

