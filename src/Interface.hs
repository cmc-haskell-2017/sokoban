module Interface where

import Types

motionManager :: Motion -> Interface -> Interface
motionManager _ interface = interface

generateInterface :: Interface
generateInterface = Interface {
        buttons = [buttonGo, buttonScore, buttonLobos]
    }

buttonGo :: Button
buttonGo = ("Go", (\x -> x), True)

buttonScore :: Button
buttonScore = ("Score", (\x -> x), False)

buttonLobos :: Button
buttonLobos = ("Lobos", (\x -> x), False)
