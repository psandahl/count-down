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
import Game.Board exposing (Board, BoardWidth(..), GameWidth(..))
import Game.Board as Board
import Game.Camera exposing (Camera)
import Game.Camera as Camera
import Game.Level exposing (Level)
import Game.Marker exposing (Marker)
import Game.Marker as Marker
import Game.MeshStore exposing (MeshStore)
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
import WebGL as GL
import WebGL.Texture exposing (Texture)


type alias Game =
    { pMatrix : Mat4
    , camera : Camera
    , board : Board
    , marker : Marker
    , userInput : UserInput
    }


type alias Speed =
    Float


new : Level -> MeshStore -> Array Texture -> Game
new level meshStore textures =
    { pMatrix = Mat.makePerspective 45 aspectRatio 0.1 100
    , camera = Camera.init
    , board = Board.init (BoardWidth 100) (GameWidth 10) meshStore.boardMesh
    , marker = Marker.init ( 0, 0 ) meshStore.markerMesh
    , userInput = UserInput.init
    }


{-| Take care of the timeTick event. Evolve game logic.
-}
timeTick : Game -> Game
timeTick game =
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
        , GL.clearColor 1 0 0 1
        ]
        [ Attr.height height
        , Attr.width width
        ]
        [ -- The rendering order is really important. First the board must
          -- be rendered (without filling the depth buffer).
          Board.render game.pMatrix game.camera.vMatrix game.board

        -- Render the reflection, which is blending with the board.
        , Marker.renderReflection game.pMatrix game.camera.vMatrix game.marker

        -- Last render the Marker. Its color must never blend when rendering
        -- the reflection.
        , Marker.render game.pMatrix game.camera.vMatrix game.marker
        ]


aspectRatio : Float
aspectRatio =
    (toFloat width / toFloat height)


width : Int
width =
    800


height : Int
height =
    600
