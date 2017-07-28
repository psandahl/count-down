module Game.UserInput exposing (UserInput, init, setZoomIn, setZoomOut, setGoLeft)

import Keys exposing (Key)


type alias UserInput =
    { zoomIn : Bool
    , zoomOut : Bool
    , goLeft : Bool
    }


init : UserInput
init =
    { zoomIn = False
    , zoomOut = False
    , goLeft = False
    }


setZoomIn : Bool -> UserInput -> UserInput
setZoomIn value userInput =
    { userInput | zoomIn = value }


setZoomOut : Bool -> UserInput -> UserInput
setZoomOut value userInput =
    { userInput | zoomOut = value }


setGoLeft : Bool -> UserInput -> UserInput
setGoLeft value userInput =
    { userInput | goLeft = value }
