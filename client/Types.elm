module Types exposing (State(..), Model, Msg(..))

{-| The runtime state of the application.
-}

import Array exposing (Array)
import Game.MeshStore exposing (MeshStore)
import WebGL.Texture exposing (Error, Texture)


type State
    = Null
    | ReadyForPlay
    | Playing
    | GameOver
    | Error String


type alias Model =
    { state : State
    , textures : Array Texture
    , meshStore : MeshStore
    }


type Msg
    = TexturesLoaded (Result Error (List Texture))
