module GameLog
    exposing
        ( GameLog
        , LogMessage(..)
        , init
        , add
        , view
        , win
        , lose
        , countDown
        )

import Array exposing (Array)
import Array
import Game.Types exposing (Seconds)
import Html exposing (Html)
import Html
import Html.Attributes as Attr


{-| GameLog type. Opaque to the user.
-}
type GameLog
    = GameLog (List ( String, String ))


{-| LogMessage type.
-}
type LogMessage
    = InfoMessage String
    | Greeting String
    | SadMessage String


{-| Initialize an empty GameLog.
-}
init : GameLog
init =
    GameLog []


{-| Add a new message to the log. Maintain the max length of the GameLog.
-}
add : LogMessage -> GameLog -> GameLog
add logMessage (GameLog xs) =
    GameLog <| List.take 10 (entry logMessage :: xs)


{-| View the GameLog.
-}
view : GameLog -> Html msg
view (GameLog xs) =
    Html.div
        [ Attr.class "gamelog"
        ]
        (List.map viewMessage <|
            List.reverse xs
        )


{-| Generate a win message using the random seed.
-}
win : Int -> LogMessage
win =
    genMessage
        (Array.fromList
            [ "Well done!", "Gratz!", ":-)" ]
        )
        Greeting


{-| Generate a lose message using the random seed.
-}
lose : Int -> LogMessage
lose =
    genMessage
        (Array.fromList
            [ "Oh noes!", "Lost that one.", ":-(" ]
        )
        SadMessage


{-| Add a count down message.
-}
countDown : Seconds -> GameLog -> GameLog
countDown timeLeft gameLog =
    if timeLeft < 4 then
        add (InfoMessage <| toString timeLeft) gameLog
    else
        gameLog


genMessage : Array String -> (String -> LogMessage) -> Int -> LogMessage
genMessage strings ctor rand =
    let
        index =
            rand % Array.length strings
    in
        case Array.get index strings of
            Just msg ->
                ctor msg

            Nothing ->
                ctor "--- INTERNAL ERROR - GAMELOG ---"


viewMessage : ( String, String ) -> Html msg
viewMessage ( color, msg ) =
    Html.div [ Attr.style [ ( "color", color ) ] ] [ Html.text msg ]


entry : LogMessage -> ( String, String )
entry logMessage =
    case logMessage of
        InfoMessage m ->
            ( "#eeeeec", m )

        Greeting m ->
            ( "#8ae234", m )

        SadMessage m ->
            ( "#ef2929", m )
