module Example.Instance.Tiles exposing (..)


import Array exposing (Array)
import Dict exposing (Dict)
import Color exposing (Color)
import Set exposing (Set)


import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Kvant.Vec2 exposing (..)
import Kvant.Plane as Plane exposing (empty)
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver as Solver exposing (Step(..))
import Kvant.Solver.Options exposing (Approach(..))
import Kvant.Solver.Options as Solver exposing (Options)

import Example.Advance exposing (Status(..), AdvanceMode(..))
import Example.Instance.Tiles.Plane exposing (TileKey, TileGrid, noTile)
import Example.Instance.Tiles.Plane as TilesPlane exposing (make)


type TilingRules
    = FromGrid TileSet TileGrid
    | FromRules ()


type alias TileSet = ( Int -> Maybe TileKey, TileKey -> Maybe Int )


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
