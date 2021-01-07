module Example.Instance.Tiles.Plane exposing (..)

import Array exposing (Array)


import Kvant.Vec2 exposing (Vec2, loadSize)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)


type alias TileKey = String


type alias TileGrid = Array (Array TileKey)


type alias TilesPlane = Plane Vec2 TileKey


noTile : TileKey
noTile = "none"


make : TileGrid -> TilesPlane
make tileGrid =
    makeInBounds
        (loadSize tileGrid |> Maybe.withDefault (0, 0))
        tileGrid


makeInBounds : Vec2 -> TileGrid -> TilesPlane
makeInBounds ( width, height ) tileGrid =
    Plane
        ( width, height )
        (\(x, y) ->
            if (x < width) && (y < height) then
                tileGrid
                    |> Array.get y
                    |> Maybe.andThen (Array.get x)
            else Nothing
        )


toGrid : TilesPlane -> List (List TileKey)
toGrid plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault noTile)


merge : List TileKey -> ( TileKey, Int )
merge tiles =
    ( List.head tiles |> Maybe.withDefault noTile
    , List.length tiles
    )
