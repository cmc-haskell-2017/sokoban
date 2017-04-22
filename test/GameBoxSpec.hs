module GameBoxSpec (main, spec) where

import Test.Hspec
import GameBox
import Types

main :: IO ()
main = hspec spec

getTestGameBox :: GameBox
getTestGameBox = GameBox {
    gameMap = [
        WALL, WALL, WALL, 
        BOX, EMPTY, BOX, 
        WALL, WALL, WALL
    ],
    width   = 3,
    height  = 3,
}

spec :: Spec
spec = do
    let gb = (getTestGameBox)
    describe "Sokoban" $ do
        describe "GameBox" $ do
            describe "getCell" $ do
                it "TOP RIGHT" $ do
                    (getCell gb 4 4) `shouldBe` WALL
            describe "getCell" $ do
                it "CENTER LEFT"  $ do
                    (getCell gb 0 1) `shouldBe` BOX
            describe "getCell" $ do
                it "CENTER CENTER"  $ do
                    (getCell gb 1 1) `shouldBe` EMPTY
            describe "getCell" $ do
                it "CENTER RIGHT"  $ do
                    (getCell gb 2 1) `shouldBe` BOX
            describe "getCell" $ do
                it "DOWN LEFT"  $ do
                    (getCell gb 0 0) `shouldBe` WALL

            describe "pos2index" $ do
                it "(2,0) -> 9" $ do
                    (pos2index (2,0) gb) `shouldBe` 9
            describe "pos2index" $ do
                it "(0,0) -> 7" $ do
                    (pos2index (0,0) gb) `shouldBe` 7
            describe "pos2index" $ do
                it "(1,1) -> 5" $ do
                    (pos2index (1,1) gb) `shouldBe` 5
            describe "pos2index" $ do
                it "(2,2) -> 3" $ do
                    (pos2index (2,2) gb) `shouldBe` 3
            describe "pos2index" $ do
                it "(0,2) -> 1" $ do
                    (pos2index (0,2) gb) `shouldBe` 1

            describe "index2pos" $ do
                it "9 -> (2,0)" $ do
                    (index2pos 9 gb) `shouldBe` (2,0)
            describe "index2pos" $ do
                it "7 -> (0,0)" $ do
                    (index2pos 7 gb) `shouldBe` (0,0)
            describe "index2pos" $ do
                it "5 -> (1,1)" $ do
                    (index2pos 5 gb) `shouldBe` (1,1)
            describe "index2pos" $ do
                it "3 -> (2,2)" $ do
                    (index2pos 3 gb) `shouldBe` (2,2)
            describe "index2pos" $ do
                it "1 -> (0,2)" $ do
                    (index2pos 1 gb) `shouldBe` (0,2)


