module Game.Camera exposing (Camera, init, mouseMoved)

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as Vec
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Mouse exposing (Position)


{-| The Camera.
-}
type alias Camera =
    { settings : Settings
    , vMatrix : Mat4
    }


{-| The settings that make up the exact camera position.
-}
type alias Settings =
    { -- The unit base vector for camera positioning. Shall always be up.
      baseVector : Vec3

    -- The magnitude of the base vector, i.e. the camera's distance.
    , magnitude : Float

    -- The yaw (y) rotate. Angle in degrees.
    , yaw : Float

    -- The pitch (x) rotate. Angle in degrees.
    , pitch : Float
    }


{-| Initialize the camera.
-}
init : Camera
init =
    let
        settings =
            { baseVector = up
            , magnitude = 20
            , yaw = 0
            , pitch = clampPitch 65
            }
    in
        { settings = settings
        , vMatrix = makeMatrix settings
        }


{-| Handle a mouse move event. Repositioning the camera.
-}
mouseMoved : Position -> Position -> Camera -> Camera
mouseMoved from to camera =
    let
        settings =
            camera.settings

        deltaX =
            to.x - from.x

        deltaY =
            to.y - from.y

        yaw =
            settings.yaw + (toFloat -deltaX / rotationSpeed * 360)

        pitch =
            clampPitch <| settings.pitch + (toFloat -deltaY / rotationSpeed * 360)

        newSettings =
            { settings
                | yaw = yaw
                , pitch = pitch
            }
    in
        { camera
            | settings = newSettings
            , vMatrix = makeMatrix newSettings
        }


{-| The speed of rotation. The mouse must move 1000 pixels for a rotation of
360 degrees.
-}
rotationSpeed : Float
rotationSpeed =
    1000


{-| Clamp the pitch angle. The camera must never be under the surface.
-}
clampPitch : Float -> Float
clampPitch =
    clamp 10 80


makeMatrix : Settings -> Mat4
makeMatrix settings =
    let
        scaledVector =
            Vec.scale settings.magnitude settings.baseVector

        pitchMatrix =
            Mat.makeRotate (degrees settings.pitch) (vec3 1 0 0)

        yawMatrix =
            Mat.makeRotate (degrees settings.yaw) (vec3 0 1 0)

        eye =
            Mat.transform (Mat.mul yawMatrix pitchMatrix) scaledVector
    in
        Mat.makeLookAt eye origo up


origo : Vec3
origo =
    vec3 0 0 0


up : Vec3
up =
    vec3 0 1 0
