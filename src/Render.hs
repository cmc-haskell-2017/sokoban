module Render where

import Types
import GameBox(getCell)

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

scalingCoefficient = 0.5

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
giveImage images EMPTY  = (empty images)

render :: Images -> GameBox -> Picture
render set gb = centerPicture (width gb) (height gb) (pictures (listPictures set gb (listCoordinates (width gb) (height gb))))

-- функция listCoordinates принимает в себя 2 параметра: ширину и высоту игрового поля
-- обратно она возвращает список координатных пар, которые идут по порядку
-- координаты сначала увеличиватся по х, потому по у
listCoordinates :: Int -> Int -> [(Int, Int)]
listCoordinates x y = foldl (++) [] listoflists
    where
        -- listfuncs это список функций, которые принимают в себя х и возвращают (х, у)
        -- по сути это список функций, каждая из которых превращает список х в слой
        listfuncs = map (\yy -> (\xx -> (xx, yy))) [0..y-1]
        -- listoflists это список слоев - его осталось просто разгладить в один большой список
        listoflists = map (\f -> map f [0..x-1]) listfuncs

listPictures :: Images -> GameBox -> [(Int, Int)] -> [Picture]
listPictures _ _ [] = []
listPictures set gb ((x, y) : rest) = [(translate xx yy (giveImage set (getCell gb x y)))] ++ (listPictures set gb rest)
    where
        scaller = picSize * scalingCoefficient
        xx = scaller * (fromIntegral x)
        yy = scaller * (fromIntegral y)

picSize = 128

centerPicture :: Int -> Int -> Picture -> Picture
centerPicture x y pic = translate xx yy pic
    where
        xx = - scalingCoefficient * picSize * (fromIntegral x) / 2
        yy = - scalingCoefficient * picSize * (fromIntegral y) / 2
