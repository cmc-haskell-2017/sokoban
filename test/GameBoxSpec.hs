module GameBoxSpec (main, spec) where

import Test.Hspec
import GameBox
import Types

main :: IO ()
main = hspec spec

getTestGameBox :: GameBox
getTestGameBox = GameBox {
    gameMap = [WALL, WALL, WALL, BOX, EMPTY, BOX, WALL, WALL, WALL],
    width   = 3,
    height  = 3
}

spec :: Spec
spec = do
    describe "Sokoban" $ do
        describe "GameBox" $ do
            describe "getCell" $ do
                it "TOP RIGHT" $ do
                    (getCell (getTestGameBox) 4 4) `shouldBe` WALL

            describe "getCell" $ do
                it "CENTER LEFT"  $ do
                    (getCell (getTestGameBox) 0 1) `shouldBe` BOX

            describe "getCell" $ do
                it "CENTER CENTER"  $ do
                    (getCell (getTestGameBox) 1 1) `shouldBe` EMPTY

            describe "getCell" $ do
                it "DOWN LEFT"  $ do
                     (getCell (getTestGameBox) 0 0)  `shouldBe` WALL
