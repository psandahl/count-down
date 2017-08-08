module Game
    exposing
        ( Game
        , Verdict(..)
        , new
        , timeTick
        , animate
        , mouseMoved
        , keyPressed
        , keyReleased
        , render
        )

import Array exposing (Array)
import Game.Board exposing (Board)
import Game.Board as Board
import Game.Camera exposing (Camera)
import Game.Camera as Camera
import Game.Counters exposing (Counters)
import Game.Counters as Counters
import Game.Level exposing (Level, Details)
import Game.Level as Level
import Game.Marker exposing (Marker)
import Game.Marker as Marker
import Game.MeshStore exposing (MeshStore)
import Game.Types exposing (BoardWidth(..), GameWidth(..), Speed, Seconds)
import Game.UserInput exposing (UserInput)
import Game.UserInput as UserInput
import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Keys exposing (Key(..))
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Mouse exposing (Position)
import Time exposing (Time)
import WebGL exposing (Entity)
import WebGL as GL
import WebGL.Texture exposing (Texture)


type alias Game =
    { pMatrix : Mat4
    , camera : Camera
    , board : Board
    , counters : Counters
    , marker : Marker
    , markerBoost : Float
    , userInput : UserInput
    , timeLeft : Seconds
    , currentLevel : Level
    }


type Verdict
    = Continue Int
    | Won Int
    | Lose Int


new : Level -> MeshStore -> Array Texture -> Game
new level meshStore textures =
    let
        details =
            Level.details level
    in
        { pMatrix = Mat.makePerspective 45 aspectRatio 0.1 100
        , camera = Camera.init details.cameraDistance
        , board = Board.init details.boardWidth details.gameWidth meshStore.boardMesh
        , counters = Counters.init level meshStore textures
        , marker = Marker.init details.markerStart meshStore.markerMesh
        , markerBoost = clampMarkerBoost 1.5
        , userInput = UserInput.init
        , timeLeft = details.duration
        , currentLevel = level
        }


{-| Take care of the timeTick event. Evolve game logic.
-}
timeTick : ( Int, Int ) -> Game -> ( Game, Verdict )
timeTick randoms game =
    let
        ( newCounters, ( expired, stopped ) ) =
            Counters.tickTime randoms game.counters

        newTimeLeft =
            game.timeLeft - 1

        newGame =
            { game
                | counters = newCounters
                , timeLeft = newTimeLeft
            }
    in
        if newTimeLeft == 0 then
            ( newGame, Won stopped )
        else if expired > 0 then
            ( newGame, Lose stopped )
        else
            ( newGame, Continue stopped )


{-| Take care of the animate event. Animate subcomponents and evolve game
logic. Adjust the marker boost.
-}
animate : Time -> Game -> Game
animate time game =
    let
        newPosition =
            newMarkerPosition time (markerBaseMoveSpeed * game.markerBoost) game

        movedMarker =
            Marker.moveTo newPosition game.marker

        ( markedCounters, someStopped ) =
            Counters.markerMoved newPosition game.counters

        boost =
            game.markerBoost
                - time
                + if someStopped then
                    markerBoostValue
                  else
                    0
    in
        { game
            | camera = Camera.animate time game.userInput game.camera
            , counters = Counters.animate time markedCounters
            , marker =
                Marker.yaw (time * (markerBaseYawSpeed * game.markerBoost)) movedMarker
            , markerBoost = clampMarkerBoost boost
        }


{-| Take care of the mouseMoved event. Repositioning the Camera.
-}
mouseMoved : Position -> Position -> Game -> Game
mouseMoved from to game =
    { game
        | camera = Camera.mouseMoved from to game.camera
    }


{-| Take care of the key pressed event.
-}
keyPressed : Key -> Game -> Game
keyPressed =
    keyAction True


{-| Take care of the key released event.
-}
keyReleased : Key -> Game -> Game
keyReleased =
    keyAction False


newMarkerPosition : Time -> Speed -> Game -> ( Float, Float )
newMarkerPosition time speed game =
    Board.clampPosition game.board <|
        moveMarker game.userInput.goLeft time speed moveLeft <|
            moveMarker game.userInput.goUp time speed moveUp <|
                moveMarker game.userInput.goRight time speed moveRight <|
                    moveMarker game.userInput.goDown time speed moveDown <|
                        Marker.position game.marker


moveMarker :
    Bool
    -> Time
    -> Speed
    -> (Float -> ( Float, Float ) -> ( Float, Float ))
    -> ( Float, Float )
    -> ( Float, Float )
moveMarker active time speed move pos =
    if active then
        move (time * speed) pos
    else
        pos


moveLeft : Float -> ( Float, Float ) -> ( Float, Float )
moveLeft amount ( x, z ) =
    ( x - amount, z )


moveUp : Float -> ( Float, Float ) -> ( Float, Float )
moveUp amount ( x, z ) =
    ( x, z - amount )


moveRight : Float -> ( Float, Float ) -> ( Float, Float )
moveRight amount ( x, z ) =
    ( x + amount, z )


moveDown : Float -> ( Float, Float ) -> ( Float, Float )
moveDown amount ( x, z ) =
    ( x, z + amount )


markerBaseMoveSpeed : Float
markerBaseMoveSpeed =
    2


markerBaseYawSpeed : Float
markerBaseYawSpeed =
    90


clampMarkerBoost : Float -> Float
clampMarkerBoost =
    clamp 1 7


markerBoostValue : Float
markerBoostValue =
    1


keyAction : Bool -> Key -> Game -> Game
keyAction value key game =
    case key of
        Left ->
            { game | userInput = UserInput.setGoLeft value game.userInput }

        Up ->
            { game | userInput = UserInput.setGoUp value game.userInput }

        Right ->
            { game | userInput = UserInput.setGoRight value game.userInput }

        Down ->
            { game | userInput = UserInput.setGoDown value game.userInput }

        Plus ->
            { game | userInput = UserInput.setZoomIn value game.userInput }

        Minus ->
            { game | userInput = UserInput.setZoomOut value game.userInput }

        SomethingElse ->
            game


render : Game -> Html msg
render game =
    GL.toHtmlWith
        [ GL.antialias
        , GL.depth 1
        , GL.clearColor 0 0 0 0
        ]
        [ Attr.height height
        , Attr.width width
        , Attr.class "game"
        ]
    <|
        entities game


entities : Game -> List Entity
entities game =
    let
        pMatrix =
            game.pMatrix

        vMatrix =
            game.camera.vMatrix

        board =
            Board.render pMatrix vMatrix game.board

        counters =
            Counters.render pMatrix vMatrix game.counters

        marker =
            [ -- Render the reflection, which is blending with the board and
              -- the counters.
              Marker.renderReflection pMatrix vMatrix game.marker

            -- Last render the Marker. Its color must never blend when rendering
            -- the reflection.
            , Marker.render pMatrix vMatrix game.marker
            ]
    in
        -- The order is important. First the board, then the counters and
        -- last the marker in the internal order described above.
        board :: counters ++ marker


aspectRatio : Float
aspectRatio =
    (toFloat width / toFloat height)


width : Int
width =
    1024


height : Int
height =
    768
