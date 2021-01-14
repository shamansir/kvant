module Example.Instance.Tiles.Plane exposing (..)

import Array exposing (Array)


import Kvant.Vec2 exposing (Vec2, loadSize)
import Kvant.Plane exposing (..)
import Kvant.Plane as Plane exposing (fromArray2d)


type alias TileKey = String


type alias TileGrid = Array (Array TileKey)


type alias TilesPlane = Plane TileKey


noTile : TileKey
noTile = "none"


make : TileGrid -> TilesPlane
make tileGrid =
    Plane.fromArray2d
        (loadSize tileGrid |> Maybe.withDefault (0, 0))
        tileGrid


toGrid : TilesPlane -> List (List TileKey)
toGrid plane =
    toList2d plane
        |> List.map (List.map <| Maybe.withDefault noTile)


merge : List TileKey -> ( TileKey, Int )
merge tiles =
    ( List.head tiles |> Maybe.withDefault noTile
    , List.length tiles
    )
