module Game.Counters exposing (Counters, init, tickTime, animate, render, counterCoords)

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
tickTime ( probability, slot ) counters =
    if testProbability counters.details.probability probability then
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
            toFloat <| index % w

        col =
            toFloat <| index // w

        mid =
            toFloat w / 2
    in
        ( row - mid + 0.5, col - mid + 0.5 )
