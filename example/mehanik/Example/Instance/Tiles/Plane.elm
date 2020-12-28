module Example.Instance.Tiles.Plane exposing (..)

import Array exposing (Array)


import Kvant.Vec2 exposing (Vec2, loadSize)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)


type alias TileId = Int


type alias TilesPlane = Plane Vec2 TileId


make : Array (Array TileId) -> TilesPlane
make tileGrid =
    makeInBounds
        (loadSize tileGrid |> Maybe.withDefault (0, 0))
        tileGrid


makeInBounds : Vec2 -> Array (Array TileId) -> TilesPlane
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


toGrid : TilesPlane -> List (List TileId)
toGrid plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault -1)
