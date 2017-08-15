module Game.Level
    exposing
        ( Level
        , Details
        , init
        , next
        , isLast
        , details
        , asInt
        )

import Array
import Game.Types exposing (BoardWidth(..), GameWidth(..), Probability(..), Seconds)


type Level
    = Level Int


{-| Record with details related to a level.
-}
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


{-| Initialize the Level.
-}
init : Level
init =
    Level 1


{-| Step to the next Level.
-}
next : Level -> Level
next (Level n) =
    Level <| n + 1


{-| Tell if the level is the last supported level.
-}
isLast : Level -> Bool
isLast (Level n) =
    n >= 15


{-| Get the details for the Level.
-}
details : Level -> Details
details level =
    let
        ( bw, gw, cd, prob ) =
            levelStuff level
    in
        { boardWidth = bw
        , gameWidth = gw
        , markerStart = ( 0, 0 )
        , cameraDistance = cd
        , probability = prob
        , duration = 30
        }


{-| Turn a Level to an Int.
-}
asInt : Level -> Int
asInt (Level n) =
    n


{-| Hardcoded values for the level stuff that varies.
-}
levelStuff : Level -> ( BoardWidth, GameWidth, Float, Probability )
levelStuff level =
    let
        levelAsInt =
            asInt level

        boardIndex =
            (levelAsInt - 1) // 3

        ( bw, gw, cd ) =
            boardStuff boardIndex
    in
        ( bw, gw, cd, OneTo <| 3 - (levelAsInt - 1) % 3 )


boardStuff : Int -> ( BoardWidth, GameWidth, Float )
boardStuff index =
    let
        stuff =
            Array.fromList
                [ ( BoardWidth 6, GameWidth 5, 10 )
                , ( BoardWidth 9, GameWidth 8, 16 )
                , ( BoardWidth 11, GameWidth 10, 20 )
                , ( BoardWidth 13, GameWidth 12, 24 )
                , ( BoardWidth 16, GameWidth 15, 30 )
                ]
    in
        Maybe.withDefault ( BoardWidth 16, GameWidth 15, 30 ) <| Array.get index stuff
