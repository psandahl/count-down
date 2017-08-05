module Main exposing (main)

import AnimationFrame
import Array
import Game
import Game.Level as Level
import Game.MeshStore as MeshStore
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as Events
import Keyboard
import Keys
import Mouse
import Random
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
      , game = Nothing
      , timeDiff = 0
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
                        , state = ReadyForPlay Level.init
                      }
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | state = Error "Can't load textures" }
                    , Cmd.none
                    )

        Second ->
            ( model
            , Random.generate TimeTick <|
                Random.pair (Random.int 0 100) (Random.int 0 3571)
            )

        TimeTick randoms ->
            case model.game of
                Just game ->
                    case Game.timeTick randoms game of
                        ( newGame, Game.Continue points ) ->
                            ( { model
                                | game = Just newGame
                              }
                            , Cmd.none
                            )

                        ( newGame, Game.Won points ) ->
                            ( { model
                                | game = Nothing
                                , state = ReadyForPlay <| Level.next (newGame.currentLevel)
                              }
                            , Cmd.none
                            )

                        ( newGame, Game.Lose points ) ->
                            ( { model
                                | game = Nothing
                                , state = GameOver
                              }
                            , Cmd.none
                            )

                Nothing ->
                    ( model, Cmd.none )

        Animate diff ->
            ( { model
                | game = Maybe.map (Game.animate diff) model.game
                , timeDiff = diff
              }
            , Cmd.none
            )

        StartNewGame level ->
            ( { model
                | game = Just <| Game.new level model.meshStore model.textures
                , state = Playing level
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
                , game =
                    Maybe.map (Game.mouseMoved model.mousePosition position) model.game
                , mousePosition = position
              }
            , Cmd.none
            )

        MouseMoved position ->
            ( { model
                | game =
                    Maybe.map (Game.mouseMoved model.mousePosition position) model.game
                , mousePosition = position
              }
            , Cmd.none
            )

        KeyPressed key ->
            ( { model
                | game = Maybe.map (Game.keyPressed key) model.game
              }
            , Cmd.none
            )

        KeyReleased key ->
            ( { model
                | game = Maybe.map (Game.keyReleased key) model.game
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "fullscreen" ]
        [ case model.state of
            ReadyForPlay level ->
                Html.div [ Attr.class "splash" ]
                    [ Html.h1
                        [ Attr.class "splash"
                        , Events.onClick <| StartNewGame level
                        ]
                        [ Html.text <|
                            "Start level: "
                                ++ toString (Level.asInt level)
                        ]
                    ]

            Playing level ->
                case model.game of
                    Just game ->
                        Html.div [ Attr.class "game" ]
                            [ Game.render game ]

                    Nothing ->
                        Html.h1 [ Attr.class "shout" ]
                            [ Html.text "Unexpected error!" ]

            Error msg ->
                Html.h1 [ Attr.class "shout" ] [ Html.text msg ]

            _ ->
                Html.h1 [ Attr.class "shout" ] [ Html.text "Loading ..." ]
        ]


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
