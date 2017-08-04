module Game.Counter
    exposing
        ( Vertex
        , Counter
        , State(..)
        , init
        , advanceState
        , stop
        , fade
        , render
        , makeMesh
        )

import Array exposing (Array, get)
import Math.Matrix4 exposing (Mat4)
import Math.Matrix4 as Mat
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector3 as Vec
import Time exposing (Time)
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
    , currentTexture : Int
    , bgColor : Vec3
    , fgColor : Vec3
    , colorFade : Float
    , state : State
    }


{-| The runtime state of a counter.
-}
type
    State
    -- The counter is counting.
    = Counting Int
      -- The counter is expired. Wait the specified number of seconds before
      -- go to Finished.
    | Expired Int
      -- The counter is stopped by the marker. Wait the specified number
      -- of the seconds before go to Finished.
    | Stopped Int
      -- The counter is finished and shall be removed.
    | Finished


{-| Initialize the Counter.
-}
init : ( Float, Float ) -> Array Texture -> Mesh Vertex -> Counter
init ( x, z ) textures mesh =
    { mesh = mesh
    , modelMatrix = Mat.makeTranslate3 x 0 z
    , textures = textures
    , currentTexture = 10
    , bgColor = vec3 0 0 0
    , fgColor = green
    , colorFade = 1
    , state = Counting 10
    }


{-| TimeTick event. Advance the state machine for the counter.
-}
advanceState : Counter -> Counter
advanceState counter =
    case counter.state of
        Counting 1 ->
            { counter
                | currentTexture = 0
                , fgColor = red
                , colorFade = 1
                , state = Expired 1
            }

        Counting count ->
            { counter
                | currentTexture = count - 1
                , fgColor = mix red green (toFloat count / 10)
                , colorFade = 1
                , state = Counting (count - 1)
            }

        Expired 0 ->
            { counter | state = Finished }

        Expired count ->
            { counter | state = Expired (count - 1) }

        Stopped 0 ->
            { counter | state = Finished }

        Stopped count ->
            { counter | state = Stopped (count - 1) }

        _ ->
            counter


{-| Stop the counter if it's counting.
-}
stop : Counter -> Counter
stop counter =
    case counter.state of
        Counting count ->
            { counter
                | currentTexture = 11
                , fgColor = green
                , colorFade = 1
                , state = Stopped 1
            }

        _ ->
            counter


{-| Animation event. Fade the counter with the given time.
-}
fade : Time -> Counter -> Counter
fade time counter =
    case counter.state of
        Counting count ->
            { counter | colorFade = clamp 0 1 <| counter.colorFade - time }

        _ ->
            counter


{-| Render the counter.
-}
render : Mat4 -> Mat4 -> Counter -> Maybe Entity
render pMatrix vMatrix counter =
    case get counter.currentTexture counter.textures of
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
                        , bgColor = counter.bgColor
                        , fgColor = counter.fgColor
                        , colorFade = counter.colorFade
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


mix : Vec3 -> Vec3 -> Float -> Vec3
mix a b f =
    let
        mixFactor =
            clamp 0 1 f

        fromA =
            1 - mixFactor

        fromB =
            mixFactor
    in
        vec3 (fromA * Vec.getX a + fromB * Vec.getX b)
            (fromA * Vec.getY a + fromB * Vec.getY b)
            (fromA * Vec.getZ a + fromB * Vec.getZ b)


green : Vec3
green =
    vec3 0 (204 / 255) 0


red : Vec3
red =
    vec3 (204 / 255) 0 0


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


fragmentShader :
    Shader {}
        { uniforms
            | texture : Texture
            , bgColor : Vec3
            , fgColor : Vec3
            , colorFade : Float
        }
        { vTexCoord : Vec2 }
fragmentShader =
    [glsl|
        precision mediump float;

        uniform sampler2D texture;
        uniform vec3 bgColor;
        uniform vec3 fgColor;
        uniform float colorFade;

        varying vec2 vTexCoord;

        void main()
        {
            float alpha = texture2D(texture, vTexCoord).a;
            if (alpha > 0.5)
            {
                gl_FragColor = vec4(mix(bgColor, fgColor, colorFade), 1.0);
            }
            else
            {
                discard;
            }
        }
    |]
