module Game.Marker exposing (Vertex, Marker, init, yaw, render, makeMesh)

import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector3 exposing (Vec3, vec3, normalize, getY, setY)
import Time exposing (Time)
import WebGL exposing (Mesh, Entity, Shader)
import WebGL as GL


type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


type alias Marker =
    { mesh : Mesh Vertex
    , position : Vec3
    , pitchMatrix : Mat4
    , yawAngle : Float
    }


init : Mesh Vertex -> Marker
init mesh =
    { mesh = mesh
    , position = vec3 0 0 0
    , pitchMatrix = Mat.makeRotate (degrees 180) (vec3 1 0 0)
    , yawAngle = 0
    }


yaw : Time -> Marker -> Marker
yaw time marker =
    { marker
        | yawAngle = marker.yawAngle + time * yawSpeed
    }


render : Mat4 -> Mat4 -> Marker -> Entity
render pMatrix vMatrix marker =
    let
        mvpMatrix =
            Mat.mul pMatrix (Mat.mul vMatrix <| modelMatrixMarker marker)
    in
        GL.entity
            vertexShader
            fragmentShader
            marker.mesh
            { mvpMatrix = mvpMatrix }


modelMatrixMarker : Marker -> Mat4
modelMatrixMarker marker =
    let
        translateMatrix =
            Mat.makeTranslate <| adjustHeight 0.5 marker.position

        yawMatrix =
            Mat.makeRotate (degrees marker.yawAngle) (vec3 0 1 0)
    in
        Mat.mul translateMatrix <| Mat.mul yawMatrix marker.pitchMatrix


adjustHeight : Float -> Vec3 -> Vec3
adjustHeight amount vec =
    let
        y =
            amount + getY vec
    in
        setY y vec


yawSpeed : Float
yawSpeed =
    90


{-| The pyramid has no shared vertice instances as it must utilize flat shading.
The generated geometry is a pyramid, standing with the pointy top up (as
you would expect).
-}
makeMesh : Mesh Vertex
makeMesh =
    let
        vertices =
            [ -- Front side.
              { position = top, normal = frontNormal }
            , { position = frontLeft, normal = frontNormal }
            , { position = frontRight, normal = frontNormal }

            -- Left side.
            , { position = top, normal = leftNormal }
            , { position = backLeft, normal = leftNormal }
            , { position = frontLeft, normal = leftNormal }

            -- Right side.
            , { position = top, normal = rightNormal }
            , { position = frontRight, normal = rightNormal }
            , { position = backRight, normal = rightNormal }

            -- Back side.
            , { position = top, normal = backNormal }
            , { position = backRight, normal = backNormal }
            , { position = backLeft, normal = backNormal }

            -- Bottom, triangle 1.
            , { position = frontRight, normal = bottomNormal }
            , { position = frontLeft, normal = bottomNormal }
            , { position = backLeft, normal = bottomNormal }

            -- Bottom, triangle 2.
            , { position = frontRight, normal = bottomNormal }
            , { position = backLeft, normal = bottomNormal }
            , { position = backRight, normal = bottomNormal }
            ]

        indices =
            [ ( 0, 1, 2 )
            , ( 3, 4, 5 )
            , ( 6, 7, 8 )
            , ( 9, 10, 11 )
            , ( 12, 13, 14 )
            , ( 15, 16, 17 )
            ]
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


backLeft : Vec3
backLeft =
    vec3 -0.5 -0.5 -0.5


backRight : Vec3
backRight =
    vec3 0.5 -0.5 -0.5


frontNormal : Vec3
frontNormal =
    normalize <| vec3 0 2 1


leftNormal : Vec3
leftNormal =
    normalize <| vec3 -1 2 0


rightNormal : Vec3
rightNormal =
    normalize <| vec3 1 2 0


backNormal : Vec3
backNormal =
    normalize <| vec3 0 2 -1


bottomNormal : Vec3
bottomNormal =
    vec3 0 -1 0


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
