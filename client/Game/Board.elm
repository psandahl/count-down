module Game.Board exposing (Vertex, Board, makeMesh)

{-| The Board module implements everything needed for a Board instance.
-}

import Math.Vector3 exposing (Vec3, vec3)
import WebGL as GL
import WebGL exposing (Mesh)


{-| The vertex type for a Board. Only position is needed.
-}
type alias Vertex =
    { position : Vec3
    }


type alias Board =
    { mesh : Mesh Vertex
    }


{-| Make a unit sized board with center at origo.
-}
makeMesh : Mesh Vertex
makeMesh =
    GL.indexedTriangles
        [ { position = vec3 0.5 0 -0.5 }
        , { position = vec3 -0.5 0 -0.5 }
        , { position = vec3 -0.5 0 0.5 }
        , { position = vec3 0.5 0 0.5 }
        ]
        [ ( 0, 1, 2 )
        , ( 0, 2, 3 )
        ]
