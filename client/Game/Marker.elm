module Game.Marker exposing (Vertex, Marker, init, render, makeMesh)

import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector3 exposing (Vec3, vec3, normalize)
import WebGL exposing (Mesh, Entity, Shader)
import WebGL as GL


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


type alias Marker =
    { mesh : Mesh Vertex
    }


init : Mesh Vertex -> Marker
init mesh =
    { mesh = mesh
    }


render : Mat4 -> Mat4 -> Marker -> Entity
render pMatrix vMatrix marker =
    let
        mvpMatrix =
            Mat.mul pMatrix vMatrix
    in
        GL.entity
            vertexShader
            fragmentShader
            marker.mesh
            { mvpMatrix = mvpMatrix }


{-| The pyramid has no shared vertice instances as it must utilize flat shading.
-}
makeMesh : Mesh Vertex
makeMesh =
    let
        vertices =
            [ -- Front side.
              { position = top, normal = frontNormal }
            , { position = frontLeft, normal = frontNormal }
            , { position = frontRight, normal = frontNormal }
            ]

        indices =
            [ ( 0, 1, 2 ) ]
    in
        GL.indexedTriangles vertices indices


top : Vec3
top =
    vec3 0 0.5 0


frontLeft : Vec3
frontLeft =
    vec3 -0.5 -0.5 0.5


frontRight : Vec3
frontRight =
    vec3 0.5 -0.5 0.5


frontNormal : Vec3
frontNormal =
    normalize <| vec3 0 2 1


vertexShader : Shader Vertex { uniforms | mvpMatrix : Mat4 } {}
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;

        uniform mat4 mvpMatrix;

        void main()
        {
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


fragmentShader : Shader {} uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        void main()
        {
            gl_FragColor = vec4(0.8, 0.8, 0.8, 1.0);
        }
    |]
