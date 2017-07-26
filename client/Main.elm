module Main exposing (main)

import AnimationFrame
import Array
import Game
import Game.Level as Level
import Game.MeshStore as MeshStore
import Html exposing (Html)
import Html.Events as Events
import Mouse
import Task
import Time
import Types exposing (State(..), Model, Msg(..))
import WebGL.Texture as Texture


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { state = Null
      , trackingMouse = False
      , mousePosition = { x = 0, y = 0 }
      , textures = Array.empty
      , meshStore = MeshStore.init
      , level = Level.init
      , game = Nothing
      }
      -- Load the textures.
    , loadTextures
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TexturesLoaded result ->
            case result of
                Ok ts ->
                    ( { model
                        | textures = Array.fromList ts
                        , state = ReadyForPlay
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | state = Error "Can't load textures" }
                    , Cmd.none
                    )

        TimeTick ->
            ( { model | game = Maybe.map Game.timeTick model.game }
            , Cmd.none
            )

        Animate diff ->
            ( { model | game = Maybe.map (Game.animate diff) model.game }
            , Cmd.none
            )

        StartNewGame ->
            ( { model
                | game = Just <| Game.new model.level model.meshStore model.textures
                , level = Level.nextLevel model.level
                , state = Playing
              }
            , Cmd.none
            )

        MousePressed position ->
            ( { model
                | trackingMouse = True
                , mousePosition = position
              }
            , Cmd.none
            )

        MouseReleased position ->
            ( { model
                | trackingMouse = False
                , mousePosition = position
              }
            , Cmd.none
            )

        MouseMoved position ->
            ( { model
                | mousePosition = position
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.state of
        Error err ->
            Html.text err

        ReadyForPlay ->
            Html.div []
                [ Html.button [ Events.onClick StartNewGame ] [ Html.text "Start new game" ]
                , Html.p [] []
                , Html.text <| "Tracking: " ++ toString model.trackingMouse
                , Html.p [] []
                , Html.text <| "Position: " ++ toString model.mousePosition
                ]

        Playing ->
            case model.game of
                Just game ->
                    Game.render game

                Nothing ->
                    Html.text "Should never happen ..."

        _ ->
            Html.text "Waiting ..."


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        animationSubs =
            if model.state == Playing then
                [ Time.every Time.second (always TimeTick)
                , AnimationFrame.diffs Animate
                ]
            else
                []

        mbSubs =
            [ Mouse.downs MousePressed, Mouse.ups MouseReleased ]

        mvSubs =
            if model.trackingMouse then
                [ Mouse.moves MouseMoved ]
            else
                []
    in
        Sub.batch <| animationSubs ++ mbSubs ++ mvSubs


{-| Attempt load all the needed textures from the server.
-}
loadTextures : Cmd Msg
loadTextures =
    Task.attempt TexturesLoaded <|
        Task.sequence <|
            List.map Texture.load
                [ "texture/ten.png"
                ]
