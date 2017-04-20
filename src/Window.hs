module Window where

import Types
import GameBox
import Render(renderGameBox, renderInterface) 
import Interface

import Graphics.Gloss.Interface.Pure.Game

data WindowTag = GAME | INTERFACE

instance Eq WindowTag where
    (==) GAME GAME              = True
    (==) INTERFACE INTERFACE    = True
    (==) _ _                    = False

data Window = Window {
    tag :: WindowTag,
    game :: GameBox,
    ui :: Interface
}

generateWindow :: String -> IO Window
generateWindow path = do 
   gameBox <- generateBox path
   return Window {
   tag = INTERFACE,
   game = gameBox
--ui = 7
}

renderWindow :: Images -> Window -> Picture
renderWindow images window
    | tag window == GAME        = renderGameBox images (game window)
    | tag window == INTERFACE   = renderInterface images (ui window)

motionManager :: Motion -> Window -> Window
motionManager motion window 
    | tag window == GAME = Window {
        tag = tag window,
        game = GameBox.motionManager motion (game window),
        ui = ui window
    }
    | tag window == INTERFACE = Window {
        tag = tag window,
        game = game window,
        ui = Interface.motionManager motion (ui window)
    }

