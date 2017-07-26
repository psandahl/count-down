module Game exposing (Game, new, timeTick, animate, mouseMoved, render)

import Array exposing (Array)
import Game.Board exposing (Board, BoardWidth(..), InnerWidth(..))
import Game.Board as Board
import Game.Camera exposing (Camera)
import Game.Camera as Camera
import Game.Level exposing (Level)
import Game.MeshStore exposing (MeshStore)
import Html exposing (Html)
import Html
import Html.Attributes as Attr
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Mouse exposing (Position)
import Time exposing (Time)
import WebGL as GL
import WebGL.Texture exposing (Texture)


type alias Game =
    { pMatrix : Mat4
    , camera : Camera
    , board : Board
    }


new : Level -> MeshStore -> Array Texture -> Game
new level meshStore textures =
    { pMatrix = Mat.makePerspective 45 aspectRatio 0.1 100
    , camera = Camera.init
    , board = Board.init (BoardWidth 10) (InnerWidth 5) meshStore.boardMesh
    }


timeTick : Game -> Game
timeTick game =
    game


animate : Time -> Game -> Game
animate time game =
    game


mouseMoved : Position -> Position -> Game -> Game
mouseMoved from to game =
    { game
        | camera = Camera.mouseMoved from to game.camera
    }


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
