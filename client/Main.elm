module Main exposing (main)

import AnimationFrame
import Array
import Game
import Game.Level as Level
import Game.MeshStore as MeshStore
import Html exposing (Html)
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
                    ( { model | textures = Array.fromList ts, state = ReadyForPlay }, Cmd.none )

                Err _ ->
                    ( { model | state = Error "Can't load textures" }, Cmd.none )

        TimeTick ->
            ( { model | game = Maybe.map Game.timeTick model.game }, Cmd.none )

        Animate diff ->
            ( { model | game = Maybe.map (Game.animate diff) model.game }, Cmd.none )


view : Model -> Html Msg
view model =
    case model.state of
        Error err ->
            Html.text err

        _ ->
            Html.text "hej"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        -- The subscriptions are only made while in the Playing state.
        Playing ->
            Sub.batch
                [ Time.every Time.second (always TimeTick)
                , AnimationFrame.diffs Animate
                ]

        _ ->
            Sub.none


{-| Attempt load all the needed textures from the server.
-}
loadTextures : Cmd Msg
loadTextures =
    Task.attempt TexturesLoaded <|
        Task.sequence <|
            List.map Texture.load
                [ "texture/ten.png"
                ]
