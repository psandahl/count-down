module Game.Counters exposing (Counters, init, animate, render)

import Array exposing (Array)
import Array
import Game.Counter exposing (Counter, Vertex)
import Game.Counter as Counter
import Game.Level exposing (Level, Details)
import Game.Level as Level
import Game.MeshStore exposing (MeshStore)
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Entity, Mesh)
import WebGL.Texture exposing (Texture)


type alias Counters =
    { details : Details
    , counterMesh : Mesh Vertex
    , textures : Array Texture
    , counterArray : Array Counter
    }


init : Level -> MeshStore -> Array Texture -> Counters
init level meshStore textures =
    { details = Level.details level
    , counterMesh = meshStore.counterMesh
    , textures = textures
    , counterArray = Array.fromList [ Counter.init ( 0, 0 ) textures meshStore.counterMesh ]
    }


animate : Time -> Counters -> Counters
animate time counters =
    { counters
        | counterArray = Array.map (Counter.animate time) counters.counterArray
    }


render : Mat4 -> Mat4 -> Counters -> List Entity
render pMatrix vMatrix counters =
    let
        maybes =
            Array.map (Counter.render pMatrix vMatrix) counters.counterArray
    in
        Array.foldl pickJust [] maybes


pickJust : Maybe Entity -> List Entity -> List Entity
pickJust mEntity eList =
    case mEntity of
        Just entity ->
            entity :: eList

        Nothing ->
            eList
