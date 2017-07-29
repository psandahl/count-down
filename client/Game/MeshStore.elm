module Game.MeshStore exposing (MeshStore, init)

{-| The MeshStore is a cache of already created meshes.
-}

import Game.Board as Board
import Game.Marker as Marker
import WebGL exposing (Mesh)


type alias MeshStore =
    { boardMesh : Mesh Board.Vertex
    , markerMesh : Mesh Marker.Vertex
    }


{-| Initialize the MeshStore.
-}
init : MeshStore
init =
    { boardMesh = Board.makeMesh
    , markerMesh = Marker.makeMesh
    }
