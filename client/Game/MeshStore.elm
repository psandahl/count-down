module Game.MeshStore exposing (MeshStore, init)

{-| The MeshStore is a cache of already created meshes.
-}

import Game.Board as Board
import Game.Counter as Counter
import Game.Marker as Marker
import WebGL exposing (Mesh)


type alias MeshStore =
    { boardMesh : Mesh Board.Vertex
    , counterMesh : Mesh Counter.Vertex
    , markerMesh : Mesh Marker.Vertex
    }


{-| Initialize the MeshStore.
-}
init : MeshStore
init =
    { boardMesh = Board.makeMesh
    , counterMesh = Counter.makeMesh
    , markerMesh = Marker.makeMesh
    }
