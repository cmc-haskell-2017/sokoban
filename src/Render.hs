module Render where

import Types

import Graphics.Gloss.Juicy
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate

scalingCoefficient = 0.6

scaling :: (Picture -> Picture)
scaling = (scale scalingCoefficient scalingCoefficient)

loadImages :: IO Images
loadImages = do
    Just pers <- loadJuicyPNG "img/lobos.png"
    return Images {
        box = scaling pers,
        wall = scaling pers,
        person = scaling pers,
        empty = scaling pers,
        mark = scaling pers
    }

giveImage :: Images -> Cell -> Picture
giveImage images BOX    = (box images)
giveImage images WALL   = (wall images)
giveImage images PERSON = (person images)
giveImage images GOAL   = (mark images)
giveImage images EMPTY  = (mark images)

render :: Images -> GameBox -> Picture
render image _  = pictures [
    translate 150 20 (giveImage image PERSON),
    translate 130 30 (giveImage image BOX)
    ]

-- функция coordinates принимает GameBox - берет из него список полей
-- и по этому списку формирует список пар - (координатаХ, координатаУ)
coordinates :: GameBox -> [(Int, Int)]
coordinates _ = [(0, 0)]
