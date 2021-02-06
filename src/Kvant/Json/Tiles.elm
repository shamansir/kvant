module Kvant.Json.Tiles exposing (..)


import Dict exposing (Dict)
import Json.Encode as E
import Json.Decode as D


import Kvant.Vec2 as Vec2
import Kvant.Tiles as T
import Kvant.Plane as Plane
import Kvant.Json.Adjacency as Adjacency


decode : D.Decoder T.TileAdjacency
decode =
    Adjacency.decode decodeTileKey decodeTileKey


decodeTileKey : D.Decoder ( T.TileKey, T.Rotation )
decodeTileKey =
    D.map2
        Tuple.pair
        (D.field "key" D.string)
        (D.field "rotation" D.int)


encode : T.TileAdjacency -> E.Value
encode =
    Adjacency.encode encodeTileKey encodeTileKey


encodeTileKey : ( T.TileKey, T.Rotation ) -> E.Value
encodeTileKey ( key, rotation ) =
    E.object
        [ ( "key", E.string key )
        , ( "rotation", E.int rotation )
        ]

