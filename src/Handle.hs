module Handle where

import Types
import Graphics.Gloss.Interface.Pure.Game
import Window

leftDown  :: Window -> Window
leftDown  gb = gb
rightDown :: Window -> Window
rightDown gb = gb
leftUp    :: Window -> Window
leftUp    gb = gb
rightUp   :: Window -> Window 
rightUp   gb = gb

handle :: Event -> Window -> Window
handle (EventKey (SpecialKey KeyLeft) Down _ _) u   = leftDown u
handle (EventKey (SpecialKey KeyRight) Down _ _) u  = rightDown u
handle (EventKey (SpecialKey KeyLeft) Up _ _) u     = leftUp u
handle (EventKey (SpecialKey KeyRight) Up _ _) u    = rightUp u
handle _ u = u
