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


new : Level -> MeshStore -> Array Texture -> Game
new level meshStore textures =
    { pMatrix = Mat.makePerspective 45 aspectRatio 0.1 100
    , camera = Camera.init
    , board = Board.init (BoardWidth 100) (GameWidth 10) meshStore.boardMesh
    , marker = Marker.init ( 0, 0 ) meshStore.markerMesh
    , userInput = UserInput.init
    }


timeTick : Game -> Game
timeTick game =
    game


{-| Take care of the animate event. Animate subcomponents.
-}
animate : Time -> Game -> Game
animate time game =
    { game
        | camera = Camera.animate time game.userInput game.camera
        , marker = Marker.yaw time <| moveMarker time game
    }


moveMarker : Time -> Game -> Marker
moveMarker time game =
    if game.userInput.goLeft then
        let
            ( x, z ) =
                Marker.position game.marker
        in
            Marker.moveTo ( x - time, z ) game.marker
    else
        game.marker


mouseMoved : Position -> Position -> Game -> Game
mouseMoved from to game =
    { game
        | camera = Camera.mouseMoved from to game.camera
    }


keyPressed : Key -> Game -> Game
keyPressed key game =
    case key of
        Left ->
            { game | userInput = UserInput.setGoLeft True game.userInput }

        Plus ->
            { game | userInput = UserInput.setZoomIn True game.userInput }

        Minus ->
            { game | userInput = UserInput.setZoomOut True game.userInput }

        SomethingElse ->
            game


keyReleased : Key -> Game -> Game
keyReleased key game =
    case key of
        Left ->
            { game | userInput = UserInput.setGoLeft False game.userInput }

        Plus ->
            { game | userInput = UserInput.setZoomIn False game.userInput }

        Minus ->
            { game | userInput = UserInput.setZoomOut False game.userInput }

        SomethingElse ->
            game


render : Game -> Html msg
render game =
    GL.toHtmlWith
        [ GL.alpha True
        , GL.antialias
        , GL.depth 1
        , GL.clearColor 1 0 0 1
        ]
        [ Attr.height height
        , Attr.width width
        ]
        [ Board.render game.pMatrix game.camera.vMatrix game.board
        , Marker.render game.pMatrix game.camera.vMatrix game.marker
        , Marker.renderReflection game.pMatrix game.camera.vMatrix game.marker
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
