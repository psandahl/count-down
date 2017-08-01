module Game.Counter exposing (Vertex, Counter, init, render, makeMesh)

import Array exposing (Array, get)
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import WebGL exposing (Entity, Mesh, Texture, Shader)
import WebGL as GL


{-| Vertex type for the counter. Position and texCoord.
-}
type alias Vertex =
    { position : Vec3
    , texCoord : Vec2
    }


{-| The Counter.
-}
type alias Counter =
    { mesh : Mesh Vertex
    , modelMatrix : Mat4
    , textures : Array Texture
    , currentCount : Int
    }


{-| Initialize the Counter.
-}
init : ( Float, Float ) -> Array Texture -> Mesh Vertex -> Counter
init ( x, z ) textures mesh =
    { mesh = mesh
    , modelMatrix = Mat.makeTranslate3 x 0 z
    , textures = textures
    , currentCount = 0
    }


render : Mat4 -> Mat4 -> Counter -> Maybe Entity
render pMatrix vMatrix counter =
    case get counter.currentCount counter.textures of
        Just texture ->
            let
                mvpMatrix =
                    Mat.mul pMatrix <| Mat.mul vMatrix counter.modelMatrix
            in
                Just <|
                    GL.entity
                        vertexShader
                        fragmentShader
                        counter.mesh
                        { mvpMatrix = mvpMatrix
                        , texture = texture
                        }

        Nothing ->
            Nothing


{-| Make a Mesh, which is a unit sized square lying at y = zero.
-}
makeMesh : Mesh Vertex
makeMesh =
    GL.indexedTriangles
        [ { position = vec3 0.5 0 -0.5, texCoord = vec2 1 1 }
        , { position = vec3 -0.5 0 -0.5, texCoord = vec2 0 1 }
        , { position = vec3 -0.5 0 0.5, texCoord = vec2 0 0 }
        , { position = vec3 0.5 0 0.5, texCoord = vec2 1 0 }
        ]
        [ ( 0, 1, 2 ), ( 0, 2, 3 ) ]


vertexShader :
    Shader Vertex
        { uniforms | mvpMatrix : Mat4 }
        { vTexCoord : Vec2
        }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec2 texCoord;

        uniform mat4 mvpMatrix;

        varying vec2 vTexCoord;

        void main()
        {
            vTexCoord = texCoord;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} { uniforms | texture : Texture } { vTexCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;

        varying vec2 vTexCoord;

        void main()
        {
            gl_FragColor = texture2D(texture, vTexCoord);
        }
    |]
