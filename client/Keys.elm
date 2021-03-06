module Keys exposing (Key(..), fromKeyCode)

import Keyboard exposing (KeyCode)


type Key
    = Plus
    | Minus
    | Left
    | Up
    | Right
    | Down
    | SomethingElse


fromKeyCode : KeyCode -> Key
fromKeyCode code =
    case code of
        37 ->
            Left

        38 ->
            Up

        39 ->
            Right

        40 ->
            Down

        65 ->
            Left

        68 ->
            Right

        83 ->
            Down

        87 ->
            Up

        107 ->
            Plus

        109 ->
            Minus

        _ ->
            SomethingElse
