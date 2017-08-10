module Types exposing (State(..), Model, Msg(..), Progress(..))

{-| The runtime state of the application.
-}

import Array exposing (Array)
import Game exposing (Game)
import Game.Level exposing (Level)
import Game.MeshStore exposing (MeshStore)
import Keys exposing (Key)
import GameLog exposing (GameLog)
import Mouse exposing (Position)
import Time exposing (Time)
import WebGL.Texture exposing (Error, Texture)


{-| The application's runtime state.
-}
type
    State
    -- The state before things are initialized.
    = Null
      -- All resources are loaded.
    | Loaded
      -- Initialization done. Ready for play.
    | ReadyForPlay Progress Level
      -- Ongoing game.
    | Playing Level
      -- The game is over.
    | GameOver
      -- Some catastrofic error.
    | Error String


{-| The progress at start of new game. Level up to next level or replay
the current level.
-}
type Progress
    = LevelUp
    | Replay


{-| The application's model.
-}
type alias Model =
    { -- The runtime state.
      state : State

    -- Are we tracking mouse moves?
    , trackingMouse : Bool

    -- The latest known mouse position.
    , mousePosition : Position

    -- Cache of the textures needed for the counters.
    , textures : Array Texture

    --  Cache of the meshes needed for the game.
    , meshStore : MeshStore

    -- The ongoing game (if any).
    , game : Maybe Game

    -- The number of lives left (when reaching zero game is over).
    , lives : Int

    -- The accumulated number of points.
    , points : Int

    -- Game log.
    , gameLog : GameLog

    -- Store the timediff for the last frame. For debug etc.
    , timeDiff : Float
    }


{-| The application's message type.
-}
type
    Msg
    -- The requested textures are loaded.
    = TexturesLoaded (Result Error (List Texture))
      -- Fire every second while in Playing state.
    | Second
      -- Every second generate a time tick event with a pair of random
      -- numbers, the first in range [0, 100], the other in range [0, maxInt].
    | TimeTick ( Int, Int )
      -- Time difference in fractions of s since the last animation event.
    | Animate Time
      -- Start the game for the first time.
    | Start
      -- Request to start a new game.
    | StartNewGame Level
      -- The mouse button is pressed.
    | MousePressed Position
      -- The mouse button is released.
    | MouseReleased Position
      -- The mouse is moved.
    | MouseMoved Position
      -- A key on the keyboard is pressed.
    | KeyPressed Key
      -- A key on the keyboard is released.
    | KeyReleased Key
