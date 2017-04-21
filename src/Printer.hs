module Printer where

import Types

printCell     :: Cell -> String
printBox      :: GameBox -> String
insertElem    :: Int -> a -> [a] -> [a]

insertElem _ _  [] = []
insertElem i e x = (take i x) ++ [e] ++ insertElem i e (drop i x)

printBox gb = foldl (++) "" (insertElem (width gb) "\n" (map printCell (gameMap gb)))


printCell EMPTY   = " "
printCell WALL    = "#"
printCell BOX     = "@"
printCell GOAL    = "X"
printCell PERSON  = "&"
