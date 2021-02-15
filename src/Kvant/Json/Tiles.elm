module Kvant.Json.Tiles exposing (..)


import Dict exposing (Dict)
import Json.Encode as E
import Json.Decode as D


import Kvant.Vec2 as Vec2
import Kvant.Tiles as T
import Kvant.Rotation as T
import Kvant.Rotation as Rotation
import Kvant.Plane as Plane
import Kvant.Json.Adjacency as Adjacency
import Kvant.Adjacency as Adjacency


decode : D.Decoder T.TileAdjacency
decode =
    Adjacency.decodeWith decodeTileKey decodeTileKey
        |> D.map (Adjacency.map <| Tuple.mapSecond Rotation.fromId)


decodeTileKey : D.Decoder ( T.TileKey, T.RotationId )
decodeTileKey =
    D.map2
        Tuple.pair
        (D.field "key" D.string)
        (D.field "rotation" D.int)


encode : T.TileAdjacency -> E.Value
encode =
    Adjacency.map (Tuple.mapSecond Rotation.toId)
        >> Adjacency.encodeWith encodeTileKey encodeTileKey



encodeTileKey : ( T.TileKey, T.RotationId ) -> E.Value
encodeTileKey ( key, rotation ) =
    E.object
        [ ( "key", E.string key )
        , ( "rotation", E.int rotation )
        ]

