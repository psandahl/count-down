module Types exposing (State(..), Model, Msg(..))

{-| The runtime state of the application.
-}

import Array exposing (Array)
import Game.MeshStore exposing (MeshStore)
import WebGL.Texture exposing (Error, Texture)


{-| The application's runtime state.
-}
type State
    = Null
      -- ^ The state before things are initialized.
    | ReadyForPlay
      -- ^ Initialization done. Ready for playing.
    | Playing
      -- ^ Ongoing game.
    | GameOver
      -- ^ The game is over.
    | Error String


{-| The application's model.
-}
type alias Model =
    { state : State
    , textures : Array Texture
    , meshStore : MeshStore
    }


{-| The application's message type.
-}
type Msg
    = TexturesLoaded (Result Error (List Texture))
      -- ^ The requested textures are loaded.
    | TimeTick
      -- ^ TimeTick every second while in Playing state.
    | Animate Float



-- ^ Time difference since last frame.
