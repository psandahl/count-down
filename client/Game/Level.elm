module Game.Level exposing (Level, Details, init, nextLevel, details, asInt)

import Game.Types exposing (BoardWidth(..), GameWidth(..))


type Level
    = Level Int


type alias Details =
    { boardWidth : BoardWidth
    , gameWidth : GameWidth
    , markerStart : ( Float, Float )
    }


init : Level
init =
    Level 1


nextLevel : Level -> Level
nextLevel (Level n) =
    Level <| n + 1


details : Level -> Details
details level =
    { boardWidth = BoardWidth 11
    , gameWidth = GameWidth 10
    , markerStart = ( 0, 0 )
    }


asInt : Level -> Int
asInt (Level n) =
    n
