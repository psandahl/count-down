module Game.Board
    exposing
        ( Vertex
        , Board
        , BoardWidth(..)
        , InnerWidth(..)
        , init
        , render
        , makeMesh
        )

{-| The Board module implements everything needed for a Board instance.
-}

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
    , innerRadius : Float
    }


{-| Width of the board. This is a scale factor, and it will scale the
board in x, y and z dimensions equally. Shall have no effect on y though.
-}
type BoardWidth
    = BoardWidth Float


{-| Inner width of the playing field on the board.
-}
type InnerWidth
    = InnerWidth Float


{-| Create a new Board. Use an already made mesh from the store.
-}
init : BoardWidth -> InnerWidth -> Mesh Vertex -> Board
init (BoardWidth outer) (InnerWidth inner) mesh =
    { mesh = mesh
    , modelMatrix = Mat.makeScale3 outer outer outer
    , innerRadius = inner / 2 -- Just use the radius.
    }


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
            , innerRadius = board.innerRadius
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
            | innerRadius : Float
        }
        { vPosition : Vec3
        }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform float innerRadius;

        varying vec3 vPosition;

        const float thickness = 0.5;

        void main()
        {
            float x = abs(vPosition.x);
            float z = abs(vPosition.z);

            if (x >= innerRadius && x < innerRadius + thickness && z < innerRadius + thickness)
            {
                gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0);
            }
            else if (z >= innerRadius && z < innerRadius + thickness && x < innerRadius)
            {
                gl_FragColor = vec4(0.0, 1.0, 0.0, 1.0);
            }
            else
            {
                gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
            }
        }
    |]
