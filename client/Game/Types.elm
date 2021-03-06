module Game.Types
    exposing
        ( BoardWidth(..)
        , GameWidth(..)
        , Probability(..)
        , Speed
        , Seconds
        , testProbability
        )

{-| Width of the board. This is a scale factor, and it will scale the
board in x, y and z dimensions equally. Shall have no effect on y though.
-}


type BoardWidth
    = BoardWidth Int


{-| Width of the playing field on the board.
-}
type GameWidth
    = GameWidth Int


{-| Probability.
-}
type Probability
    = OneTo Int


{-| Speed, units per second.
-}
type alias Speed =
    Float


{-| Alias for a time count in seconds.
-}
type alias Seconds =
    Int


testProbability : Probability -> Int -> Bool
testProbability (OneTo p) x =
    x % p == 0
