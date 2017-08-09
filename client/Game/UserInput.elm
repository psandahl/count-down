module Game.UserInput
    exposing
        ( UserInput
        , init
        , setZoomIn
        , setZoomOut
        , setGoLeft
        , setGoUp
        , setGoRight
        , setGoDown
        )


type alias UserInput =
    { zoomIn : Bool
    , zoomOut : Bool
    , goLeft : Bool
    , goUp : Bool
    , goRight : Bool
    , goDown : Bool
    }


init : UserInput
init =
    { zoomIn = False
    , zoomOut = False
    , goLeft = False
    , goUp = False
    , goRight = False
    , goDown = False
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


setGoUp : Bool -> UserInput -> UserInput
setGoUp value userInput =
    { userInput | goUp = value }


setGoRight : Bool -> UserInput -> UserInput
setGoRight value userInput =
    { userInput | goRight = value }


setGoDown : Bool -> UserInput -> UserInput
setGoDown value userInput =
    { userInput | goDown = value }
