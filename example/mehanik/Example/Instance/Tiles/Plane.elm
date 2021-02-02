module Example.Instance.Tiles.Plane exposing (..)

import Array exposing (Array)


import Kvant.Vec2 as Vec2
import Kvant.Plane as Plane
import Kvant.Plane exposing (Plane)
import Kvant.Tiles exposing (noTile, TileKey, Rotation, TileGrid)


type alias TilesPlane = Plane (TileKey, Rotation)


make : TileGrid -> TilesPlane
make tileGrid =
    Plane.fromArray2d
        (Vec2.loadSize tileGrid |> Maybe.withDefault (0, 0))
        tileGrid


toGrid : TilesPlane -> List (List (TileKey, Rotation))
toGrid plane =
    Plane.toList2d plane
        |> List.map (List.map <| Maybe.withDefault ( noTile, 0 ))


merge : List (TileKey, Rotation) -> ( (TileKey, Rotation), Int )
merge tiles =
    ( List.head tiles |> Maybe.withDefault ( noTile, 0 )
    , List.length tiles
    )
