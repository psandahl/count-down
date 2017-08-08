module GameLog
    exposing
        ( GameLog
        , LogMessage(..)
        , init
        , add
        , view
        , gratz
        , win
        , lose
        )

import Array
import Html exposing (Html)
import Html
import Html.Attributes as Attr


type GameLog
    = GameLog (List ( String, String ))


type LogMessage
    = InfoMessage String
    | Greeting String
    | SadMessage String


init : GameLog
init =
    GameLog []


add : LogMessage -> GameLog -> GameLog
add logMessage (GameLog xs) =
    GameLog <| List.take 10 (entry logMessage :: xs)


view : GameLog -> Html msg
view (GameLog xs) =
    Html.div
        [ Attr.class "gamelog"
        ]
        (List.map viewMessage <|
            List.reverse xs
        )


gratz : Int -> LogMessage
gratz rand =
    let
        xs =
            Array.fromList [ "Nice!", "Great!", "Wow!", "Gratz!", "Yay!", ":-)" ]

        index =
            rand % Array.length xs
    in
        case Array.get index xs of
            Just msg ->
                Greeting msg

            Nothing ->
                Greeting "You should never see this!"


win : Int -> LogMessage
win rand =
    let
        xs =
            Array.fromList [ "Hey, you won!", "The winner takes it all!" ]

        index =
            rand % Array.length xs
    in
        case Array.get index xs of
            Just msg ->
                Greeting msg

            Nothing ->
                Greeting "You should never see this!"


lose : Int -> LogMessage
lose rand =
    let
        xs =
            Array.fromList [ "Noes!", "F*ck!", "What are you doing?", ":-(" ]

        index =
            rand % Array.length xs
    in
        case Array.get index xs of
            Just msg ->
                SadMessage msg

            Nothing ->
                SadMessage "You should never see this!"


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
