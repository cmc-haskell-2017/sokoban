module Handle where

import Types
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Pure.Simulate

leftDown  :: World -> World
leftDown  gb = gb
rightDown :: World -> World
rightDown gb = gb
leftUp    :: World -> World
leftUp    gb = gb
rightUp   :: World -> World
rightUp   gb = gb

handle :: Event -> World -> World
handle (EventKey (SpecialKey KeyLeft) Down _ _) u   = leftDown u
handle (EventKey (SpecialKey KeyRight) Down _ _) u  = rightDown u
handle (EventKey (SpecialKey KeyLeft) Up _ _) u     = leftUp u
handle (EventKey (SpecialKey KeyRight) Up _ _) u    = rightUp u
handle _ u = u
