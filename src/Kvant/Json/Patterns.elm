module Kvant.Json.Patterns exposing (..)

import Dict exposing (Dict)
import Json.Encode as E
import Json.Decode as D


import Kvant.Vec2 as Vec2
import Kvant.Patterns as P
import Kvant.Plane as Plane
import Kvant.Json.Adjacency as Adjacency


decode : D.Decoder P.UniquePatterns
decode =
    Adjacency.decode decodePattern


decodePattern : D.Decoder P.Pattern
decodePattern =
    D.array (D.array D.int)
        |> D.map
            (\grid ->
                Plane.fromArray2d
                    (Vec2.loadSize grid |> Maybe.withDefault (0, 0))
                    grid
            )


encode : P.UniquePatterns -> E.Value
encode =
    Adjacency.encode encodePattern


encodePattern : P.Pattern -> E.Value
encodePattern =
    Plane.toList2d
        >> E.list
            (E.list (Maybe.withDefault -1 >> E.int))

