module Keys exposing (Key(..), fromKeyCode)

import Keyboard exposing (KeyCode)


type Key
    = Plus
    | Minus
    | SomethingElse


fromKeyCode : KeyCode -> Key
fromKeyCode code =
    case code of
        107 ->
            Plus

        109 ->
            Minus

        _ ->
            SomethingElse
