module Keys exposing (Key(..), fromKeyCode)

import Keyboard exposing (KeyCode)


type Key
    = Plus
    | Minus
    | Left
    | SomethingElse


fromKeyCode : KeyCode -> Key
fromKeyCode code =
    case code of
        37 ->
            Left

        107 ->
            Plus

        109 ->
            Minus

        _ ->
            SomethingElse
