module Game.Counters
    exposing
        ( Counters
        , init
        , tickTime
        , markerMoved
        , animate
        , render
        , counterCoords
        )

import Array exposing (Array)
import Dict exposing (Dict)
import Dict
import Game.Counter exposing (Counter, Vertex)
import Game.Counter as Counter
import Game.Level exposing (Level, Details)
import Game.Level as Level
import Game.MeshStore exposing (MeshStore)
import Game.Types exposing (GameWidth(..), testProbability)
import Math.Matrix4 exposing (Mat4)
import Time exposing (Time)
import WebGL exposing (Entity, Mesh)
import WebGL.Texture exposing (Texture)


type alias Counters =
    { details : Details
    , counterMesh : Mesh Vertex
    , textures : Array Texture
    , counterMap : Dict Int Counter
    }


init : Level -> MeshStore -> Array Texture -> Counters
init level meshStore textures =
    { details = Level.details level
    , counterMesh = meshStore.counterMesh
    , textures = textures
    , counterMap = Dict.empty
    }


tickTime : ( Int, Int ) -> Counters -> Counters
tickTime randoms counters =
    addNewCounter randoms <| advanceCounters counters


markerMoved : ( Float, Float ) -> Counters -> Counters
markerMoved coord counters =
    let
        index =
            coordIndex counters.details.gameWidth coord
    in
        { counters
            | counterMap = Dict.update index (Maybe.map Counter.stop) counters.counterMap
        }


stopCounter : Maybe Counter -> Maybe Counter
stopCounter counter =
    Maybe.map Counter.stop counter


animate : Time -> Counters -> Counters
animate time counters =
    { counters
        | counterMap =
            Dict.map (\key counter -> Counter.animate time counter) counters.counterMap
    }


render : Mat4 -> Mat4 -> Counters -> List Entity
render pMatrix vMatrix counters =
    let
        maybes =
            Dict.map (\key counter -> Counter.render pMatrix vMatrix counter) counters.counterMap
    in
        Dict.foldl pickJust [] maybes


pickJust : Int -> Maybe Entity -> List Entity -> List Entity
pickJust key mEntity eList =
    case mEntity of
        Just entity ->
            entity :: eList

        Nothing ->
            eList


randomIndex : Int -> GameWidth -> Int
randomIndex slot (GameWidth w) =
    slot % (w * w)


{-| Given a GameWidth and an index into the board, produce coordinates for
the counter. The coordinates is in the middle of the square pointed out by
the index, so they will always be at .5 something.
-}
counterCoords : GameWidth -> Int -> ( Float, Float )
counterCoords (GameWidth w) index =
    let
        row =
            toFloat <| index // w

        col =
            toFloat <| index % w

        mid =
            toFloat w / 2
    in
        ( (col - mid) + 0.5, (row - mid) + 0.5 )


coordIndex : GameWidth -> ( Float, Float ) -> Int
coordIndex (GameWidth w) ( x, z ) =
    let
        mid =
            toFloat w / 2

        col =
            floor <| mid + x

        row =
            floor <| mid + z
    in
        row * w + col


{-| Advance the counters with one time tick.
-}
advanceCounters : Counters -> Counters
advanceCounters counters =
    { counters
        | counterMap = Dict.map (\key counter -> Counter.timeTick counter) counters.counterMap
    }


{-| Add a new counter using the random pair.
-}
addNewCounter : ( Int, Int ) -> Counters -> Counters
addNewCounter ( prob, slot ) counters =
    if testProbability counters.details.probability prob then
        let
            gameWidth =
                counters.details.gameWidth

            index =
                randomIndex slot gameWidth

            coord =
                counterCoords gameWidth index
        in
            if Dict.member index counters.counterMap then
                counters
            else
                { counters
                    | counterMap =
                        Dict.insert index
                            (Counter.init coord counters.textures counters.counterMesh)
                            counters.counterMap
                }
    else
        counters
