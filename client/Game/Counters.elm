module Game.Counters exposing (Counters, init)

import Array exposing (Array)
import Array
import Game.Counter exposing (Counter, Vertex)
import Game.Level exposing (Level, Details)
import Game.Level as Level
import Game.MeshStore exposing (MeshStore)
import WebGL exposing (Mesh)
import WebGL.Texture exposing (Texture)


type alias Counters =
    { details : Details
    , counterMesh : Mesh Vertex
    , textures : Array Texture
    , counters : Array Counter
    }


init : Level -> MeshStore -> Array Texture -> Counters
init level meshStore textures =
    { details = Level.details level
    , counterMesh = meshStore.counterMesh
    , textures = textures
    , counters = Array.empty
    }
