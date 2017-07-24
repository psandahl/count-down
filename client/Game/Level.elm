module Game.Level exposing (Level, init, nextLevel, asInt)


type Level
    = Level Int


init : Level
init =
    Level 1


nextLevel : Level -> Level
nextLevel (Level n) =
    Level <| n + 1


asInt : Level -> Int
asInt (Level n) =
    n
