module Render where

import Types
import GameBox(getCell)

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

scalingCoefficient = 0.06
picHeight = 2048.0
picWidth = 1774.0

scaling :: (Picture -> Picture)
scaling = (scale scalingCoefficient scalingCoefficient)


loadImages :: IO Images
loadImages = do
    Just personImage <- loadJuicyPNG "img/highcubViolet.png"
    Just boxImage <- loadJuicyPNG "img/cubRed.png"
    Just wallImage <- loadJuicyPNG "img/cubGray.png"
    Just markImage <- loadJuicyPNG "img/cubBlue.png"
    Just emptyImage <- loadJuicyPNG "img/cub2.png"
    return Images {
        box = scaling boxImage ,
        wall = scaling wallImage ,
        person = scaling personImage ,
        empty = scaling emptyImage ,
        mark = scaling markImage 
    }

giveImage :: Images -> Cell -> Picture
giveImage images BOX     = box images
giveImage images GOODBOX = box images
giveImage images WALL    = wall images
giveImage images PERSON  = person images
giveImage images GOAL    = mark images
giveImage images EMPTY   = empty images

--заглушка
renderInterface :: Images -> Interface -> Picture
renderInterface imgs _ = giveImage imgs BOX 

renderGameBox :: Images -> GameBox -> Picture
renderGameBox set gb = centerPicture (width gb) (height gb) (pictures (listPictures set gb (listCoordinates (width gb) (height gb))))

-- функция listCoordinates принимает в себя 2 параметра: ширину и высоту игрового поля
-- обратно она возвращает список координатных пар, которые идут по порядку
-- координаты сначала увеличиватся по х, потому по у
-- reverse везде стоят для правильного порядка отрисовки при изоморфной графике
listCoordinates :: Int -> Int -> [(Int, Int)]
listCoordinates x y = foldl (++) [] listoflists
    where
        -- listfuncs это список функций, которые принимают в себя х и возвращают (х, у)
        -- по сути это список функций, каждая из которых превращает список х в слой
        listfuncs = map (\yy -> (\xx -> (xx, yy))) [0..y-1]
        -- listoflists это список слоев - его осталось просто разгладить в один большой список
        listoflists = reverse (map (\f -> map f (reverse [0..x-1])) listfuncs)

listPictures :: Images -> GameBox -> [(Int, Int)] -> [Picture]
listPictures _ _ [] = []
listPictures set gb ((x, y) : rest) =  [firstpick] ++ (listPictures set gb rest)
    where
        firstpick = (translate xxx yyy (giveImage set (getCell gb (x,y))))
        scaller = picHeight * scalingCoefficient
        xyscale = picWidth / picHeight
        xx = scaller * (fromIntegral x)
        yy = scaller * (fromIntegral y)
        xxx = xyscale * (xx / 2 - yy / 2)
        yyy = (xx  + yy) / 4

centerPicture :: Int -> Int -> Picture -> Picture
centerPicture x y pic = translate xx yy pic
    where
        xx = - scalingCoefficient * picWidth * (fromIntegral x) / 8
        yy = - scalingCoefficient * picHeight * (fromIntegral y) / 4
