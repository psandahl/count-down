module Types exposing (State(..), Model, Msg(..))

{-| The runtime state of the application.
-}

import Array exposing (Array)
import Game exposing (Game)
import Game.Level exposing (Level)
import Game.MeshStore exposing (MeshStore)
import Time exposing (Time)
import WebGL.Texture exposing (Error, Texture)


{-| The application's runtime state.
-}
type
    State
    -- The state before things are initialized.
    = Null
      -- Initialization done. Ready for playing.
    | ReadyForPlay
      -- Ongoing game.
    | Playing
      -- The game is over.
    | GameOver
      -- Some catastrofic error.
    | Error String


{-| The application's model.
-}
type alias Model =
    { -- The runtime state.
      state : State

    -- Cache of the textures needed for the counters.
    , textures : Array Texture

    --  Cache of the meshes needed for the game.
    , meshStore : MeshStore

    -- The level of the next game to play.
    , level : Level

    -- The ongoing game (if any).
    , game : Maybe Game
    }


{-| The application's message type.
-}
type
    Msg
    -- The requested textures are loaded.
    = TexturesLoaded (Result Error (List Texture))
      -- TimeTick every second while in Playing state.
    | TimeTick
      -- Time difference in ms since the last animation event.
    | Animate Time
      -- Request to start a new game.
    | StartNewGame



-- ^ Time difference since last frame.
