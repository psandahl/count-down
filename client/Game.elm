module Game
    exposing
        ( Game
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
import Game.Types exposing (BoardWidth(..), GameWidth(..), Speed)
import Game.UserInput exposing (UserInput)
import Game.UserInput as UserInput
import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Keys exposing (Key(..))
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector3 as Vec
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
    , userInput : UserInput
    }


new : Level -> MeshStore -> Array Texture -> Game
new level meshStore textures =
    let
        details =
            Level.details level
    in
        { pMatrix = Mat.makePerspective 45 aspectRatio 0.1 100
        , camera = Camera.init
        , board = Board.init details.boardWidth details.gameWidth meshStore.boardMesh
        , counters = Counters.init level meshStore textures
        , marker = Marker.init details.markerStart meshStore.markerMesh
        , userInput = UserInput.init
        }


{-| Take care of the timeTick event. Evolve game logic.
-}
timeTick : ( Int, Int ) -> Game -> Game
timeTick randoms game =
    game


{-| Take care of the animate event. Animate subcomponents and evolve game
logic.
-}
animate : Time -> Game -> Game
animate time game =
    let
        newPosition =
            newMarkerPosition time markerMoveSpeed game

        movedMarker =
            Marker.moveTo newPosition game.marker
    in
        { game
            | camera = Camera.animate time game.userInput game.camera
            , counters = Counters.animate time game.counters
            , marker = Marker.yaw time movedMarker
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


markerMoveSpeed : Speed
markerMoveSpeed =
    3


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
