module Game.UserInput exposing (UserInput, init, setZoomIn, setZoomOut)

import Keys exposing (Key)


type alias UserInput =
    { zoomIn : Bool
    , zoomOut : Bool
    }


init : UserInput
init =
    { zoomIn = False
    , zoomOut = False
    }


setZoomIn : Bool -> UserInput -> UserInput
setZoomIn value userInput =
    { userInput | zoomIn = value }


setZoomOut : Bool -> UserInput -> UserInput
setZoomOut value userInput =
    { userInput | zoomOut = value }
