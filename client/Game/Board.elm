module Game.Board
    exposing
        ( Vertex
        , Board
        , init
        , clampPosition
        , render
        , makeMesh
        )

{-| The Board module implements everything needed for a Board instance.
-}

import Game.Types exposing (BoardWidth(..), GameWidth(..))
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import WebGL as GL
import WebGL exposing (Entity, Mesh, Shader)
import WebGL.Settings.DepthTest as Depth


{-| The vertex type for a Board. Only position is needed.
-}
type alias Vertex =
    { position : Vec3
    }


{-| A Board instance. Always with its center at world space origo.
-}
type alias Board =
    { mesh : Mesh Vertex
    , modelMatrix : Mat4
    , gameRadius : Float
    }


{-| Create a new Board. Use an already made mesh from the store.
-}
init : BoardWidth -> GameWidth -> Mesh Vertex -> Board
init (BoardWidth outer) (GameWidth inner) mesh =
    { mesh = mesh
    , modelMatrix = Mat.makeScale3 (toFloat outer) (toFloat outer) (toFloat outer)
    , gameRadius = (toFloat inner) / 2 -- Just use the radius.
    }


{-| Clamp the given position within the borders of the game area of the board.
-}
clampPosition : Board -> ( Float, Float ) -> ( Float, Float )
clampPosition board ( x, z ) =
    let
        r =
            board.gameRadius
    in
        ( clamp -r (r - 0.01) x, clamp -r (r - 0.01) z )


render : Mat4 -> Mat4 -> Board -> Entity
render pMatrix vMatrix board =
    let
        mvpMatrix =
            Mat.mul pMatrix <| Mat.mul vMatrix board.modelMatrix
    in
        GL.entityWith
            [ Depth.always { write = False, near = 0, far = 1 } ]
            vertexShader
            fragmentShader
            board.mesh
            { mvpMatrix = mvpMatrix
            , modelMatrix = board.modelMatrix
            , gameRadius = board.gameRadius
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


vertexShader :
    Shader Vertex
        { uniforms
            | mvpMatrix : Mat4
            , modelMatrix : Mat4
        }
        { vPosition : Vec3
        }
vertexShader =
    [glsl|
        attribute vec3 position;

        uniform mat4 mvpMatrix;
        uniform mat4 modelMatrix;

        varying vec3 vPosition;

        void main()
        {
            vPosition = (modelMatrix * vec4(position, 1.0)).xyz;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader :
    Shader {}
        { uniforms
            | gameRadius : Float
        }
        { vPosition : Vec3
        }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float gameRadius;

        varying vec3 vPosition;

        vec3 boardColor = vec3(0.0);
        vec3 borderColor = vec3(0.0, 0.0, 0.3);
        const float thickness = 0.25;

        void main()
        {
            float x = abs(vPosition.x);
            float z = abs(vPosition.z);

            if (x >= gameRadius && x < gameRadius + thickness && z < gameRadius + thickness)
            {
                gl_FragColor = vec4(borderColor, 1.0);
            }
            else if (z >= gameRadius && z < gameRadius + thickness && x < gameRadius)
            {
                gl_FragColor = vec4(borderColor, 1.0);
            }
            else
            {
                gl_FragColor = vec4(boardColor, 1.0);
            }
        }
    |]
