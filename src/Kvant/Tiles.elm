module Kvant.Tiles exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Set exposing (Set)



import Kvant.Plane exposing (Plane)


type alias TileKey = String


type Rotation
    = Times Int


type Symmetry
    = I | X | L | T | S -- S meaning Slope


type alias TileGrid = Array (Array TileKey)


type alias TilesPlane = Plane TileKey


type alias TileSet =
    ( Dict Int TileKey
    , Dict TileKey Int
    )


noTile = "none"


buildTileset : Set TileKey -> TileSet
buildTileset =
    Set.toList
        >> List.indexedMap
            (\index key ->
                ( ( index, key )
                , ( key, index )
                )
            )
        >> (\list ->
                ( List.map Tuple.first <| list
                , List.map Tuple.second <| list
                )
           )
        >> Tuple.mapBoth Dict.fromList Dict.fromList


toIndexInSet : TileSet -> TileKey -> Int
toIndexInSet ( _, toIndex ) key =
    Dict.get key toIndex |> Maybe.withDefault -1


fromIndexInSet : TileSet -> Int -> TileKey
fromIndexInSet ( fromIndex , _ ) key =
    Dict.get key fromIndex |> Maybe.withDefault noTile


toIndexGrid : TileSet -> Array (Array TileKey) -> Array (Array Int)
toIndexGrid tileSet =
    Array.map << Array.map <| toIndexInSet tileSet


fromIndexGrid : TileSet -> Array (Array Int) -> Array (Array TileKey)
fromIndexGrid tileSet =
    Array.map << Array.map <| fromIndexInSet tileSet

