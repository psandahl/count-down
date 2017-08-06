module Main exposing (main)

import AnimationFrame
import Array
import Game exposing (Game, Verdict(..))
import Game
import Game.Level exposing (Level)
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
      , lives = 3
      , points = 0
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
                    ( advanceGame randoms game model, Cmd.none )

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


advanceGame : ( Int, Int ) -> Game -> Model -> Model
advanceGame randoms game model =
    let
        ( newGame, verdict ) =
            Game.timeTick randoms game

        thisLevel =
            newGame.currentLevel
    in
        case verdict of
            Continue p ->
                { model
                    | game = Just newGame
                    , points = model.points + p
                }

            Won p ->
                { model
                    | game = Nothing
                    , points = model.points + p
                    , state = ReadyForPlay <| Level.next thisLevel
                }

            Lose p ->
                if model.lives > 1 then
                    { model
                        | game = Nothing
                        , points = model.points + p
                        , lives = model.lives - 1
                        , state = ReadyForPlay thisLevel
                    }
                else
                    { model
                        | game = Nothing
                        , points = model.points + p
                        , lives = 0
                        , state = GameOver
                    }


view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ case model.state of
            ReadyForPlay level ->
                viewSplash level model

            Playing level ->
                case model.game of
                    Just game ->
                        viewGame level game model

                    Nothing ->
                        viewError "Error: No game. How strange ..."

            GameOver ->
                viewError "Game Over!"

            Error msg ->
                viewError msg

            Null ->
                viewError "Loading ..."
        ]


viewSplash : Level -> Model -> Html Msg
viewSplash level model =
    Html.div [ Attr.class "splash" ]
        [ Html.p
            [ Attr.class "splash"
            , Events.onClick <| StartNewGame level
            ]
            [ Html.text <|
                "Ready for level "
                    ++ (toString <| Level.asInt level)
                    ++ "?"
            ]
        , splashHUD model
        ]


viewGame : Level -> Game -> Model -> Html Msg
viewGame level game model =
    Html.div []
        [ Game.render game
        , gameHUD level game model
        ]


viewError : String -> Html Msg
viewError msg =
    Html.p [ Attr.class "error" ] [ Html.text msg ]


splashHUD : Model -> Html Msg
splashHUD model =
    Html.div [ Attr.class "overlay" ]
        [ Html.div [] [ Html.text <| "Points: " ++ toString model.points ]
        , Html.div [] [ Html.text <| "Lives: " ++ toString model.lives ]
        ]


gameHUD : Level -> Game -> Model -> Html Msg
gameHUD level game model =
    Html.div [ Attr.class "overlay" ]
        [ Html.div [] [ Html.text <| "Points: " ++ toString model.points ]
        , Html.div [] [ Html.text <| "Lives: " ++ toString model.lives ]
        , Html.div [] [ Html.text <| "Level: " ++ toString (Level.asInt level) ]
        , Html.div [] [ Html.text <| "Time: " ++ toString game.timeLeft ++ "s" ]
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
