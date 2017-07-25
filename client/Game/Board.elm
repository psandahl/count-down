module Game.Board
    exposing
        ( Vertex
        , Board
        , OuterRadius(..)
        , InnerRadius(..)
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
    , innerRadius : InnerRadius
    }


{-| Outer radius of the board. This is a scale factor, and it will scale the
board in x, y and z dimensions equally. Shall have no effect on y though.
-}
type OuterRadius
    = OuterRadius Float


{-| Inner radius of the playing field on the board.
-}
type InnerRadius
    = InnerRadius Float


{-| Create a new Board. Use an already made mesh from the store.
-}
init : OuterRadius -> InnerRadius -> Mesh Vertex -> Board
init (OuterRadius r) innerRadius mesh =
    { mesh = mesh
    , modelMatrix = Mat.makeScale3 r r r
    , innerRadius = innerRadius
    }


render : Mat4 -> Mat4 -> Board -> Entity
render pMatrix vMatrix board =
    let
        mvpMatrix =
            Mat.mul pMatrix <| Mat.mul vMatrix board.modelMatrix
    in
        GL.entity vertexShader fragmentShader board.mesh { mvpMatrix = mvpMatrix }


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
        { mvpMatrix : Mat4
        }
        { vPosition : Vec3
        }
vertexShader =
    [glsl|
        attribute vec3 position;

        uniform mat4 mvpMatrix;

        varying vec3 vPosition;

        void main()
        {
            vPosition = position;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader :
    Shader {}
        uniforms
        { vPosition : Vec3
        }
fragmentShader =
    [glsl|
        precision mediump float;

        varying vec3 vPosition;

        void main()
        {
            gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);
        }
    |]
