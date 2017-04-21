module Handle where

import Types
import Graphics.Gloss.Interface.Pure.Game
import Window


handle :: Event -> Window -> Window
handle (EventKey (SpecialKey KeyLeft) Down _ _) window   = motionManager LEFT window
handle (EventKey (SpecialKey KeyRight) Down _ _) window  = motionManager RIGHT window
handle (EventKey (SpecialKey KeyUp) Down _ _) window     = motionManager UP window
handle (EventKey (SpecialKey KeyDown) Down _ _) window   = motionManager DOWN window
handle (EventKey (Char 'h') Down _ _) window             = motionManager LEFT window
handle (EventKey (Char 'H') Down _ _) window             = motionManager LEFT window
handle (EventKey (Char 'j') Down _ _) window             = motionManager DOWN window
handle (EventKey (Char 'J') Down _ _) window             = motionManager DOWN window
handle (EventKey (Char 'k') Down _ _) window             = motionManager UP window
handle (EventKey (Char 'K') Down _ _) window             = motionManager UP window
handle (EventKey (Char 'l') Down _ _) window             = motionManager RIGHT window
handle (EventKey (Char 'L') Down _ _) window             = motionManager RIGHT window
handle (EventKey (Char 'm') Down _ _) window             = motionManager MENU window
handle (EventKey (Char 'M') Down _ _) window             = motionManager MENU window
handle _ window = window
