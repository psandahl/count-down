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
import Game.Ball exposing (Ball, Role(..))
import Game.Ball as Ball
import Game.Board exposing (Board, BoardWidth(..), InnerWidth(..))
import Game.Board as Board
import Game.Camera exposing (Camera)
import Game.Camera as Camera
import Game.Level exposing (Level)
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
    , ball : Ball
    , userInput : UserInput
    }


new : Level -> MeshStore -> Array Texture -> Game
new level meshStore textures =
    { pMatrix = Mat.makePerspective 45 aspectRatio 0.1 100
    , camera = Camera.init
    , board = Board.init (BoardWidth 10) (InnerWidth 5) meshStore.boardMesh
    , ball = Ball.init (Vec.vec3 0 0 0) Original meshStore.ballMesh
    , userInput = UserInput.init
    }


timeTick : Game -> Game
timeTick game =
    game


animate : Time -> Game -> Game
animate time game =
    { game
        | camera = Camera.animate time game.userInput game.camera
        , ball = moveBall time game
    }


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
        , Ball.render game.pMatrix game.camera.vMatrix game.ball
        ]


moveBall : Time -> Game -> Ball
moveBall time game =
    if game.userInput.goLeft then
        Ball.goLeft time game.ball
    else
        game.ball


aspectRatio : Float
aspectRatio =
    (toFloat width / toFloat height)


width : Int
width =
    800


height : Int
height =
    600
