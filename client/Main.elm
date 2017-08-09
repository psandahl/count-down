module Main exposing (main)

import AnimationFrame
import Array
import Game.MeshStore as MeshStore
import GameLog
import Html exposing (Html)
import Keyboard
import Keys
import Mouse
import Task
import Time
import Types exposing (State(..), Model, Msg(..))
import Update
import View
import WebGL.Texture as Texture


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { state = Null
      , trackingMouse = False
      , mousePosition = { x = 0, y = 0 }
      , textures = Array.empty
      , meshStore = MeshStore.init
      , game = Nothing
      , lives = 3
      , points = 0
      , gameLog = GameLog.init
      , timeDiff = 0
      }
      -- Load the textures.
    , loadTextures
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Playing level ->
            let
                animationSubs =
                    [ Time.every Time.second (always Second)
                    , AnimationFrame.diffs <| Animate << Time.inSeconds
                    ]

                mouseButtonSubs =
                    [ Mouse.downs MousePressed
                    , Mouse.ups MouseReleased
                    ]

                mouseMoveSubs =
                    if model.trackingMouse then
                        [ Mouse.moves MouseMoved ]
                    else
                        []

                keyboardSubs =
                    [ Keyboard.downs <| KeyPressed << Keys.fromKeyCode
                    , Keyboard.ups <| KeyReleased << Keys.fromKeyCode
                    ]
            in
                Sub.batch <|
                    animationSubs
                        ++ mouseButtonSubs
                        ++ mouseMoveSubs
                        ++ keyboardSubs

        _ ->
            Sub.none


{-| Attempt load all the needed textures from the server.
-}
loadTextures : Cmd Msg
loadTextures =
    Task.attempt TexturesLoaded <|
        Task.sequence <|
            List.map Texture.load
                [ "texture/dead.png"
                , "texture/one.png"
                , "texture/two.png"
                , "texture/three.png"
                , "texture/four.png"
                , "texture/five.png"
                , "texture/six.png"
                , "texture/seven.png"
                , "texture/eight.png"
                , "texture/nine.png"
                , "texture/ten.png"
                , "texture/success.png"
                ]
