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
    { boardWidth = BoardWidth 7
    , gameWidth = GameWidth 6
    , markerStart = ( 0, 0 )
    , probability = OneTo 10
    , duration = 30
    }


asInt : Level -> Int
asInt (Level n) =
    n
