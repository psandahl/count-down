module Game.Ball exposing (Ball, Vertex, Role(..), init, goLeft, render, makeMesh)

import Math.Vector3 exposing (Vec3)
import Math.Vector3 as Vec
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Surface
import Type
import WebGL exposing (Entity, Mesh, Shader)
import WebGL as GL


type alias Ball =
    { mesh : Mesh Vertex
    , role : Role
    , position : Vec3
    , modelMatrix : Mat4
    }


{-| Just use the Vertex type from the Surface library:
position : Vec3
normal : Vec3
coord : Vec2

The coord will not be used though.

-}
type alias Vertex =
    Surface.Vertex {}


type Role
    = Original
    | Reflection


{-| Create a new ball with a position. Use mesh from store.
-}
init : Vec3 -> Role -> Mesh Vertex -> Ball
init position role mesh =
    { mesh = mesh
    , role = role
    , position = adjustPosition role position
    , modelMatrix = Mat.makeTranslate <| adjustPosition role position
    }


goLeft : Float -> Ball -> Ball
goLeft amount ball =
    let
        position =
            ball.position

        newPosition =
            Vec.setX (Vec.getX position - amount) position
    in
        { ball
            | position = newPosition
            , modelMatrix = Mat.makeTranslate newPosition
        }


adjustPosition : Role -> Vec3 -> Vec3
adjustPosition role position =
    case role of
        Original ->
            Vec.setY (Vec.getY position + 1) position

        Reflection ->
            Vec.setY (Vec.getY position - 1) position


render : Mat4 -> Mat4 -> Ball -> Entity
render pMatrix vMatrix ball =
    let
        mvpMatrix =
            Mat.mul pMatrix <| Mat.mul vMatrix ball.modelMatrix
    in
        GL.entity
            vertexShader
            fragmentShader
            ball.mesh
            { mvpMatrix = mvpMatrix }


{-| Make a spherical mesh.
-}
makeMesh : Mesh Vertex
makeMesh =
    Surface.surface Type.Sphere 15 15


{-| Vertex shader for the Ball.
-}
vertexShader : Shader Vertex { mvpMatrix : Mat4 } {}
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;
        attribute vec2 coord; // Not used

        uniform mat4 mvpMatrix;

        void main()
        {
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


{-| Fragment shader for the Ball.
-}
fragmentShader : Shader {} uniforms {}
fragmentShader =
    [glsl|
        precision mediump float;

        void main()
        {
            gl_FragColor = vec4(0.9, 0.9, 0.9, 1.0);
        }
    |]
