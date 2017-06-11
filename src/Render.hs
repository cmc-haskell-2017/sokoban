module Render where

import Types
import Const
import GameBox(getCell)

import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

scalingCoefficient :: GameBox -> Float
scalingCoefficient gb = 0.3 / fromIntegral(max (width gb) (height gb))

picHeight :: Float
picHeight = 2048.0

picWidth :: Float
picWidth = 1774.0


scaling :: GameBox -> (Picture -> Picture)
scaling gb = (scale (scalingCoefficient gb) (scalingCoefficient gb))


loadImages :: IO Images
loadImages = do
    Just personImage <- loadJuicyPNG "img/highcubViolet.png"
    Just boxImage <- loadJuicyPNG "img/cubRed.png"
    Just goodImage <- loadJuicyPNG "img/cubGreen.png"
    Just wallImage <- loadJuicyPNG "img/cubGray.png"
    Just markImage <- loadJuicyPNG "img/cubBlue.png"
    Just emptyImage <- loadJuicyPNG "img/cub2.png"
    return Images {
        box = boxImage ,
        wall = wallImage ,
        person = personImage ,
        empty = emptyImage ,
        mark =  markImage ,
        good = goodImage
    }

giveImage :: Images -> GameBox -> Cell -> Picture
giveImage images gb BOX     = scaling gb (box images)
giveImage images gb GOODBOX = scaling gb (good images)
giveImage images gb WALL    = scaling gb (wall images)
giveImage images gb PERSON  = scaling gb (person images)
giveImage images gb GOAL    = scaling gb (mark images)
giveImage images gb EMPTY   = scaling gb (empty images)

--заглушка
renderInterface :: Images -> Interface -> Picture
renderInterface imgs _ = giveImage imgs winnerBox BOX 

renderGameBox :: Images -> GameBox -> Picture
renderGameBox set gb = centerPicture gb (pictures (listPictures set gb (listCoordinates (width gb) (height gb))))

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
        firstpick = (translate xxx yyy (giveImage set gb (getCell gb (x,y))))
        scaller = picHeight * (scalingCoefficient gb)
        xyscale = picWidth / picHeight
        xx = scaller * (fromIntegral x)
        yy = scaller * (fromIntegral y)
        xxx = xyscale * (xx / 2 - yy / 2)
        yyy = (xx  + yy) / 4

centerPicture :: GameBox -> Picture -> Picture
centerPicture gb pic = translate xxx yyy pic
    where
        x = (fromIntegral (width gb))
        y = (fromIntegral (height gb))
        yyy = - picHeight * (x + y - 1) * (scalingCoefficient gb) * (scalingCoefficient gb) / 2
        xxx = - picWidth * (abs (x - y)) * (scalingCoefficient gb) * (scalingCoefficient gb) / 2
