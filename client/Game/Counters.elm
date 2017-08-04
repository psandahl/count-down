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


{-| The Counters record.
-}
type alias Counters =
    { details : Details
    , counterMesh : Mesh Vertex
    , textures : Array Texture
    , counterMap : Dict Int Counter
    }


{-| Init a new Counters collection, for the given level and be using the
provided MeshStore and textures.
-}
init : Level -> MeshStore -> Array Texture -> Counters
init level meshStore textures =
    { details = Level.details level
    , counterMesh = meshStore.counterMesh
    , textures = textures
    , counterMap = Dict.empty
    }


{-| TickTime event. Advance game logic.
-}
tickTime : ( Int, Int ) -> Counters -> Counters
tickTime randoms counters =
    addNewCounter randoms <| advanceCounters counters


{-| The marker has been moved to the given position. If it's hitting a counting
Counter, the Counter is stopped.
-}
markerMoved : ( Float, Float ) -> Counters -> Counters
markerMoved coord counters =
    let
        index =
            coordIndex counters.details.gameWidth coord
    in
        { counters
            | counterMap = Dict.update index (Maybe.map Counter.stop) counters.counterMap
        }


{-| Animate the Counters. Each Counter is faded.
-}
animate : Time -> Counters -> Counters
animate time counters =
    { counters
        | counterMap =
            Dict.map (\key counter -> Counter.fade time counter) counters.counterMap
    }


{-| The the Counters to a list of entities.
-}
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


{-| From coordinates, produce an index.
-}
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
        | counterMap = Dict.map (\key counter -> Counter.advanceState counter) counters.counterMap
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
