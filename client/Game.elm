module Game exposing (Game, new, timeTick, animate, render)

import Array exposing (Array)
import Game.Level exposing (Level)
import Game.MeshStore exposing (MeshStore)
import Html exposing (Html)
import Html
import Time exposing (Time)
import WebGL.Texture exposing (Texture)


type alias Game =
    { dummy : Int
    }


new : Level -> MeshStore -> Array Texture -> Game
new level meshStore textures =
    { dummy = 0 }


timeTick : Game -> Game
timeTick game =
    game


animate : Time -> Game -> Game
animate time game =
    game


render : Game -> Html msg
render game =
    Html.text "render"
