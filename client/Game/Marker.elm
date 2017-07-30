module Game.Marker
    exposing
        ( Vertex
        , Marker
        , init
        , yaw
        , moveTo
        , position
        , render
        , renderReflection
        , makeMesh
        )

import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as Vec
import Time exposing (Time)
import WebGL exposing (Mesh, Entity, Shader)
import WebGL as GL
import WebGL.Settings as Settings
import WebGL.Settings.Blend as Blend


{-| Vertex type for the marker. Position and normal.
-}
type alias Vertex =
    { position : Vec3
    , normal : Vec3
    }


{-| The Marker.
-}
type alias Marker =
    { mesh : Mesh Vertex
    , position : Vec3
    , pitchMatrix : Mat4 -- Pitch matrix is used for flipping the pyramid.
    , yawAngle : Float
    }


{-| Initialize the Marker, with an x, z position and a mesh from the store.
-}
init : ( Float, Float ) -> Mesh Vertex -> Marker
init ( x, z ) mesh =
    { mesh = mesh
    , position = vec3 x 0 z
    , pitchMatrix = Mat.makeRotate (degrees 180) (vec3 1 0 0)
    , yawAngle = 0
    }



{- yaw and moveTo are both animate events. -}


{-| Yaw the marker proportional to the time. Matrix handling is made at
next render.
-}
yaw : Time -> Marker -> Marker
yaw time marker =
    { marker
        | yawAngle = marker.yawAngle + time * yawSpeed
    }


{-| Move the marker to the new position. Matrix handling is made at
next render.
-}
moveTo : ( Float, Float ) -> Marker -> Marker
moveTo ( x, z ) marker =
    let
        newPosition =
            vec3 x (Vec.getY marker.position) z
    in
        { marker | position = newPosition }


{-| Get the current position of the Marker.
-}
position : Marker -> ( Float, Float )
position marker =
    ( Vec.getX marker.position, Vec.getZ marker.position )


{-| Render the Marker.
-}
render : Mat4 -> Mat4 -> Marker -> Entity
render pMatrix vMatrix marker =
    let
        mvMatrix =
            Mat.mul vMatrix <| modelMatrixMarker marker

        mvpMatrix =
            Mat.mul pMatrix mvMatrix
    in
        GL.entity
            vertexShader
            fragmentShader
            marker.mesh
            { mvpMatrix = mvpMatrix
            , mvMatrix = mvMatrix
            , vMatrix = vMatrix
            , lightDirection = lightFromAbove
            , colorDamper = 1.0
            }


{-| Render the Marker reflection.
-}
renderReflection : Mat4 -> Mat4 -> Marker -> Entity
renderReflection pMatrix vMatrix marker =
    let
        mvMatrix =
            Mat.mul vMatrix <| modelMatrixReflection marker

        mvpMatrix =
            Mat.mul pMatrix mvMatrix
    in
        GL.entityWith
            [ -- Blend with the colors on the Board ...
              Blend.add Blend.one Blend.one

            -- ... but not with its own backfaces.
            , Settings.cullFace Settings.back
            ]
            vertexShader
            fragmentShader
            marker.mesh
            { mvpMatrix = mvpMatrix
            , mvMatrix = mvMatrix
            , vMatrix = vMatrix
            , lightDirection = lightFromBelow
            , colorDamper = 0.3
            }


modelMatrixMarker : Marker -> Mat4
modelMatrixMarker marker =
    let
        translateMatrix =
            Mat.makeTranslate <| adjustHeight 0.5 marker.position

        yawMatrix =
            Mat.makeRotate (degrees marker.yawAngle) (vec3 0 1 0)
    in
        Mat.mul translateMatrix <| Mat.mul yawMatrix marker.pitchMatrix


modelMatrixReflection : Marker -> Mat4
modelMatrixReflection marker =
    let
        translateMatrix =
            Mat.makeTranslate <| adjustHeight -0.5 marker.position

        yawMatrix =
            Mat.makeRotate (degrees marker.yawAngle) (vec3 0 1 0)
    in
        Mat.mul translateMatrix yawMatrix


adjustHeight : Float -> Vec3 -> Vec3
adjustHeight amount vec =
    let
        y =
            amount + Vec.getY vec
    in
        Vec.setY y vec


yawSpeed : Float
yawSpeed =
    90


lightFromAbove : Vec3
lightFromAbove =
    Vec.normalize <| vec3 3 1 -2


lightFromBelow : Vec3
lightFromBelow =
    Vec.normalize <| vec3 3 -1 -2


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



{- Helper functions for generation of model. -}


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
    Vec.normalize <| vec3 0 2 1


leftNormal : Vec3
leftNormal =
    Vec.normalize <| vec3 -1 2 0


rightNormal : Vec3
rightNormal =
    Vec.normalize <| vec3 1 2 0


backNormal : Vec3
backNormal =
    Vec.normalize <| vec3 0 2 -1


bottomNormal : Vec3
bottomNormal =
    vec3 0 -1 0


{-| The vertex shader for the Marker. Just transforming and preparing stuff
for the fragment shader. All lightning is made in view space.
-}
vertexShader :
    Shader Vertex
        { uniforms
            | mvpMatrix : Mat4
            , mvMatrix : Mat4
        }
        { vPosition : Vec3
        , vNormal : Vec3
        }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 normal;

        uniform mat4 mvpMatrix;
        uniform mat4 mvMatrix;

        varying vec3 vPosition;
        varying vec3 vNormal;

        void main()
        {
            vPosition = (mvMatrix * vec4(position, 1.0)).xyz;
            vNormal = (mvMatrix * vec4(normal, 0.0)).xyz;
            gl_Position = mvpMatrix * vec4(position, 1.0);
        }
    |]


{-| The fragment shader for the Marker. Take in varyings and the light direction.
Other lightning stuff is hard coded in the shader.
-}
fragmentShader :
    Shader {}
        { uniforms
            | vMatrix : Mat4
            , lightDirection : Vec3
            , colorDamper : Float
        }
        { vPosition : Vec3
        , vNormal : Vec3
        }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform mat4 vMatrix;
        uniform vec3 lightDirection;
        uniform float colorDamper;

        varying vec3 vPosition;
        varying vec3 vNormal;

        vec3 markerColor = vec3(152.0 / 255.0, 1.0, 152.0 / 255.0);
        vec3 lightColor = vec3(1.0, 1.0, 1.0);
        float ambientStrength = 0.8;
        float specularShine = 64.0;
        float specularStrength = 2.0;

        vec3 transformedLightDirection();
        vec3 ambientColor();
        vec3 diffuseColor();
        vec3 specularColor();

        void main()
        {
            vec3 color = markerColor * (ambientColor() + diffuseColor() + specularColor());
            gl_FragColor = vec4(colorDamper * color, 1.0);
        }

        vec3 transformedLightDirection()
        {
            return (vMatrix * vec4(lightDirection, 0.0)).xyz;
        }

        vec3 ambientColor()
        {
            return lightColor * ambientStrength;
        }

        vec3 diffuseColor()
        {
            vec3 normal = normalize(vNormal);
            float diffuse = min(dot(normal, transformedLightDirection()), 0.0);

            return lightColor * diffuse;
        }

        vec3 specularColor()
        {
            vec3 normal = normalize(vNormal);
            vec3 reflectDir = reflect(transformedLightDirection(), normal);
            vec3 viewDir = normalize(vec3(0.0) - vPosition); // Eye at 0, 0, 0.
            float specular = pow(min(dot(viewDir, reflectDir), 0.0), specularShine);

            return lightColor * specular * specularStrength;
        }
    |]
