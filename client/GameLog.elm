module GameLog exposing (GameLog, LogMessage, init, view)

import Html exposing (Html)
import Html
import Html.Attributes as Attr


type GameLog
    = GameLog (List LogMessage)


type LogMessage
    = LogMessage String String


init : GameLog
init =
    GameLog []


view : GameLog -> Html msg
view gameLog =
    Html.div
        [ Attr.class "gamelog"
        ]
        [ Html.div [] [ Html.text "Smack" ]
        ]
