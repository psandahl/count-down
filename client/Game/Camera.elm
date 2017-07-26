module Game.Camera exposing (Camera, init, mouseMoved)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Mouse exposing (Position)


type alias Camera =
    { position : Vec3
    , vMatrix : Mat4
    }


init : Camera
init =
    let
        initial =
            vec3 0 5 20

        matrix =
            Mat.makeLookAt initial origo up
    in
        { position = initial
        , vMatrix = matrix
        }


mouseMoved : Position -> Position -> Camera -> Camera
mouseMoved from to camera =
    let
        xDelta =
            to.x - from.x

        rotPosition =
            Mat.transform (cameraRotateMatrix xDelta) camera.position
    in
        { camera
            | position = rotPosition
            , vMatrix = Mat.makeLookAt rotPosition origo up
        }


cameraRotateMatrix : Int -> Mat4
cameraRotateMatrix xDelta =
    Mat.makeRotate (degrees <| toFloat -xDelta / 800 * 360) up


origo : Vec3
origo =
    vec3 0 0 0


up : Vec3
up =
    vec3 0 1 0
