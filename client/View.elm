module View exposing (view)

import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Game exposing (Game)
import Game
import Game.Level exposing (Level)
import Game.Level as Level
import GameLog
import Types exposing (Msg(..), Model, State(..), Progress(..))


{-| The top level view function.
-}
view : Model -> Html Msg
view model =
    Html.div [ Attr.class "container" ]
        [ case model.state of
            Loaded ->
                viewStart

            ReadyForPlay progress level ->
                viewSplash progress level model

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


viewStart : Html Msg
viewStart =
    Html.div [ Attr.class "splash" ]
        [ Html.div [ Attr.class "info" ]
            [ Html.h1 [ Attr.class "info" ]
                [ Html.text "-= Count Down 3 - 2 - 1 =-"
                ]
            , Html.p []
                [ Html.text "Navigate marker with arrow keys or W-A-S-D and stop counters."
                ]
            , Html.p []
                [ Html.text "You must stop the counters. Three strikes and you're out!" ]
            , Html.p []
                [ Html.text "(Zoom using +/-, move camera with mouse)" ]
            , Html.p
                [ Attr.class "infoprompt"
                , Events.onClick Start
                ]
                [ Html.text "Let's go!"
                ]
            ]
        ]


{-| The splash screen shown in between levels.
-}
viewSplash : Progress -> Level -> Model -> Html Msg
viewSplash progress level model =
    Html.div [ Attr.class "splash" ]
        [ Html.p
            [ Attr.class "splash"
            , Events.onClick <| StartNewGame level
            ]
            [ Html.text <|
                (if progress == LevelUp then
                    "Play level: "
                 else
                    "Argh! Replay level: "
                )
                    ++ (toString <| Level.asInt level)
            ]
        , viewSplashHUD model
        , GameLog.view model.gameLog
        ]


{-| The active game.
-}
viewGame : Level -> Game -> Model -> Html Msg
viewGame level game model =
    Html.div []
        [ Game.render game
        , viewGameHUD level game model
        , GameLog.view model.gameLog
        ]


viewError : String -> Html Msg
viewError msg =
    Html.p [ Attr.class "error" ] [ Html.text msg ]


viewSplashHUD : Model -> Html Msg
viewSplashHUD model =
    Html.div [ Attr.class "leftoverlay" ]
        [ Html.div [] [ Html.text <| "Points: " ++ toString model.points ]
        , Html.div [] [ Html.text <| "Lives: " ++ toString model.lives ]
        ]


viewGameHUD : Level -> Game -> Model -> Html Msg
viewGameHUD level game model =
    Html.div [ Attr.class "leftoverlay" ]
        [ Html.div [] [ Html.text <| "Points: " ++ toString model.points ]
        , Html.div [] [ Html.text <| "Lives: " ++ toString model.lives ]
        , Html.div [] [ Html.text <| "Level: " ++ toString (Level.asInt level) ]
        , Html.div [] [ Html.text <| "Time: " ++ toString game.timeLeft ++ "s" ]
        ]
