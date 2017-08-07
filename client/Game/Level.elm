module Game.Level
    exposing
        ( Level
        , Details
        , init
        , next
        , details
        , asInt
        )

import Game.Types exposing (BoardWidth(..), GameWidth(..), Probability(..), Seconds)


type Level
    = Level Int


type alias Details =
    { -- The width of the board, in units.
      boardWidth : BoardWidth

    -- The width of the game area, in units.
    , gameWidth : GameWidth

    -- Where to start with the marker (x, z).
    , markerStart : ( Float, Float )

    -- The camera's distance to the board.
    , cameraDistance : Float

    -- The probability of adding a new counter at each tick.
    , probability : Probability

    -- The duration of the game.
    , duration : Seconds
    }


init : Level
init =
    Level 1


next : Level -> Level
next (Level n) =
    Level <| n + 1


details : Level -> Details
details level =
    { boardWidth = BoardWidth 6
    , gameWidth = GameWidth 5
    , markerStart = ( 0, 0 )
    , cameraDistance = 10
    , probability = OneTo 5
    , duration = 30
    }


asInt : Level -> Int
asInt (Level n) =
    n
