module Render where

import Types

import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate

render :: Int -> GameBox -> Picture
render _ _  = pictures [ 
    (polygon [ (0, 100), (0, -20), (30, -20), (30, 0) ]), 
    (polygon [ (20, 100), (40, 100), (30, 50), (50, 60) ])
    ]
