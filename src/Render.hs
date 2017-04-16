module Render where

import Types

import Graphics.Gloss.Juicy
-- import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Interface.Pure.Simulate

scalingCoefficient :: Float
scalingCoefficient = 0.8

scaling :: (Picture -> Picture)
scaling = (scale scalingCoefficient scalingCoefficient)

loadImages :: IO Images
loadImages = do
    Just personImage <- loadJuicyPNG "img/lobos.png"
    Just boxImage <- loadJuicyPNG "img/box.png"
    Just wallImage <- loadJuicyPNG "img/wall.png"
    Just markImage <- loadJuicyPNG "img/mark.png"
    Just emptyImage <- loadJuicyPNG "img/floor.png"
    return Images {
        box = scaling boxImage ,
        wall = scaling wallImage ,
        person = scaling personImage ,
        empty = scaling emptyImage ,
        mark = scaling markImage 
    }

giveImage :: Images -> Cell -> Picture
giveImage images BOX    = (box images)
giveImage images WALL   = (wall images)
giveImage images PERSON = (person images)
giveImage images GOAL   = (mark images)
giveImage images EMPTY  = (mark images)

render :: Images -> GameBox -> Picture
render image _  = pictures [
    translate 0 0 (giveImage image PERSON),
    translate (128*scalingCoefficient) 0 (giveImage image BOX)
    ]

-- функция coordinates принимает GameBox - берет из него список полей
-- и по этому списку формирует список пар - (координатаХ, координатаУ)
coordinates :: GameBox -> [(Int, Int)]
coordinates _ = [(0, 0)]
