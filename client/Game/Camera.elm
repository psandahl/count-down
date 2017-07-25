module Game.Camera exposing (Camera, init)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat


type alias Camera =
    { vMatrix : Mat4
    }


init : Camera
init =
    { vMatrix = Mat.makeLookAt (vec3 0 5 20) (vec3 0 0 0) (vec3 0 1 0) }
