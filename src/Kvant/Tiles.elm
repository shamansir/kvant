module Kvant.Tiles exposing (..)


import Array exposing (Array)
import Dict
import Set exposing (Set)



import Kvant.Plane exposing (Plane)


type alias TileKey = String


type alias TileGrid = Array (Array TileKey)


type alias TilesPlane = Plane TileKey


type alias TileSet = ( Int -> Maybe TileKey, TileKey -> Maybe Int )


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
        >> Tuple.mapBoth
            (\dict -> \v -> Dict.get v dict)
            (\dict -> \v -> Dict.get v dict)


toIndexInSet : TileSet -> TileKey -> Int
toIndexInSet ( _, toIndex ) =
    toIndex >> Maybe.withDefault -1


fromIndexInSet : TileSet -> Int -> TileKey
fromIndexInSet ( fromIndex , _ ) =
    fromIndex >> Maybe.withDefault noTile


toIndexGrid : TileSet -> Array (Array TileKey) -> Array (Array Int)
toIndexGrid tileSet =
    Array.map << Array.map <| toIndexInSet tileSet



fromIndexGrid : TileSet -> Array (Array Int) -> Array (Array TileKey)
fromIndexGrid tileSet =
    Array.map << Array.map <| fromIndexInSet tileSet

