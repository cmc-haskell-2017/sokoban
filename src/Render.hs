module Render where

import Types

import Graphics.Gloss.Juicy
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate


scaling :: (Picture -> Picture)
scaling = (scale 0.5 0.5)

loadImages :: IO Images
loadImages = do
    Just pers <- loadJuicyPNG "img/lobos.png"
    return Images {
        box = pers,
        wall = pers,
        person = scaling pers,
        empty = pers,
        mark = pers
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
