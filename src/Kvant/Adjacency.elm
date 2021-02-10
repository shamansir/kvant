module Kvant.Adjacency exposing (..)

import Dict exposing (Dict)

import Kvant.Matches exposing (Matches)
import Kvant.Plane exposing (Offset)


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


mapKey : (comparable -> comparable) -> Adjacency comparable a -> Adjacency comparable a
mapKey f =
    Dict.toList
        >> List.map (Tuple.mapFirst f)
        >> Dict.fromList


reflective : Adjacency i a -> Adjacency i i
reflective =
    keyedMap (\k _ -> k)


get : comparable -> Adjacency comparable a -> Maybe a
get key = Dict.get key >> Maybe.map .subject
