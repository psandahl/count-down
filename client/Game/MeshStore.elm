module Game.MeshStore exposing (MeshStore, init)

{-| The MeshStore is a cache of already created meshes.
-}

import Game.Ball as Ball
import Game.Board as Board
import WebGL exposing (Mesh)


type alias MeshStore =
    { ballMesh : Mesh Ball.Vertex
    , boardMesh : Mesh Board.Vertex
    }


{-| Initialize the MeshStore.
-}
init : MeshStore
init =
    { ballMesh = Ball.makeMesh
    , boardMesh = Board.makeMesh
    }
