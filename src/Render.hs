module Render where

import Types

import Graphics.Gloss.Juicy
import Graphics.Gloss.Geometry.Line
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate


loadImages :: IO Images
loadImages = do
	Just pers <- loadJuicyPNG "img/lobos.png"
	return Images {
		box = pers,
		wall = pers,
		person = pers,
		empty = pers,
		mark = pers
	}


render :: Images -> GameBox -> Picture
render image _  = (person image)
