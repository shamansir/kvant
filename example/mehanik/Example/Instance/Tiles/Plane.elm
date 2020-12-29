module Example.Instance.Tiles.Plane exposing (..)

import Array exposing (Array)


import Kvant.Vec2 exposing (Vec2, loadSize)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)


type alias TileId = Int


type alias TileGrid = Array (Array TileId)


type alias TilesPlane = Plane Vec2 TileId


noTile : TileId
noTile = -1


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


toGrid : TilesPlane -> List (List TileId)
toGrid plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault noTile)


merge : List TileId -> TileId
merge = always noTile -- FIXME
