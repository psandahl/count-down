module Update exposing (update)

import Array
import Game
import Game exposing (Game, Verdict(..))
import Game.Level as Level
import GameLog exposing (LogMessage(..))
import GameLog
import Random
import Types exposing (Msg(..), Model, State(..), Progress(..))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TexturesLoaded result ->
            case result of
                Ok ts ->
                    ( { model
                        | textures = Array.fromList ts
                        , state = ReadyForPlay LevelUp Level.init
                        , gameLog =
                            GameLog.add (InfoMessage "Welcome to 3 - 2 - 1!") model.gameLog
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
advanceGame ( a, b ) game model =
    let
        ( newGame, verdict ) =
            Game.timeTick ( a, b ) game

        thisLevel =
            newGame.currentLevel
    in
        case verdict of
            Continue p ->
                { model
                    | game = Just newGame
                    , points = model.points + p
                    , gameLog =
                        if p > 0 then
                            GameLog.add (GameLog.gratz b) model.gameLog
                        else
                            model.gameLog
                }

            Won p ->
                { model
                    | game = Nothing
                    , points = model.points + p
                    , state = ReadyForPlay LevelUp <| Level.next thisLevel
                    , gameLog = GameLog.add (GameLog.win b) model.gameLog
                }

            Lose p ->
                if model.lives > 1 then
                    { model
                        | game = Nothing
                        , points = model.points + p
                        , lives = model.lives - 1
                        , state = ReadyForPlay Replay thisLevel
                        , gameLog = GameLog.add (GameLog.lose b) model.gameLog
                    }
                else
                    { model
                        | game = Nothing
                        , points = model.points + p
                        , lives = 0
                        , state = GameOver
                        , gameLog = GameLog.add (SadMessage "Game Over :-(") model.gameLog
                    }
