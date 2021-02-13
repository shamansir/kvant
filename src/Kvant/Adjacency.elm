module Kvant.Adjacency exposing (..)

import Dict exposing (Dict)

import Kvant.Matches exposing (Matches)
import Kvant.Matches as Matches
import Kvant.Plane exposing (Offset)
import Kvant.Neighbours as Neighbours


type alias Repetition = Int


type alias SubjectId = Int


type alias Adjacency subj_id subj =
    Dict
        subj_id
        { subject: subj
        , weight : Float
        , matches : Dict Offset (Matches subj_id)
        }



-- fromGrid : TileGrid -> Adjacency (TileKey, Rotation) (TileKey, Rotation)


map : (a -> b) -> Adjacency i a -> Adjacency i b
map =
    keyedMap << always


keyedMap : (i -> a -> b) -> Adjacency i a -> Adjacency i b
keyedMap f =
    Dict.map
        (\k v ->
            { subject = f k v.subject
            , weight = v.weight
            , matches = v.matches
            }
        )


mapKey : (comparableA -> comparableB) -> Adjacency comparableA a -> Adjacency comparableB a
mapKey f =
    Dict.toList
        >> List.map (Tuple.mapFirst f)
        >> List.map
            (Tuple.mapSecond <|
                \v ->
                    { subject = v.subject
                    , weight = v.weight
                    , matches = v.matches |> Dict.map (always <| Matches.map f)
                    }
            )
        >> Dict.fromList


reflective : Adjacency i a -> Adjacency i i
reflective =
    keyedMap (\k _ -> k)


get : comparable -> Adjacency comparable a -> Maybe a
get key = Dict.get key >> Maybe.map .subject


noMatches : Dict Offset (Matches subj_id)
noMatches =
    Neighbours.fill Matches.none |> Neighbours.toDict


merge
    :  Dict Offset (Matches comparable)
    -> Dict Offset (Matches comparable)
    -> Dict Offset (Matches comparable)
merge matchesA matchesB =
    Dict.merge
        Dict.insert
        (\key v1 v2 -> Dict.insert key <| Matches.or v1 v2)
        Dict.insert
        matchesA
        matchesB
        Dict.empty
