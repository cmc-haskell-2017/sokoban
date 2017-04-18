module Handle where

import Types
import Graphics.Gloss.Interface.Pure.Game

leftDown  :: GameBox -> GameBox
leftDown  gb = gb
rightDown :: GameBox -> GameBox
rightDown gb = gb
leftUp    :: GameBox -> GameBox
leftUp    gb = gb
rightUp   :: GameBox -> GameBox
rightUp   gb = gb

handle :: Event -> GameBox -> GameBox
handle (EventKey (SpecialKey KeyLeft) Down _ _) u   = leftDown u
handle (EventKey (SpecialKey KeyRight) Down _ _) u  = rightDown u
handle (EventKey (SpecialKey KeyLeft) Up _ _) u     = leftUp u
handle (EventKey (SpecialKey KeyRight) Up _ _) u    = rightUp u
handle _ u = u
