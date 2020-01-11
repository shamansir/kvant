module WFC.Plane.Offset exposing (..)


import Dict
import Dict exposing (Dict)


import WFC.Plane exposing (Cell, cellToMaybe)

import WFC.Vec2 exposing (..)
import WFC.Neighbours exposing (..)


type Offset v = Offset v


type OffsetPlane v a = OffsetPlane { from: v, to: v } (Offset v -> Maybe a)


get : Offset v -> OffsetPlane v a -> Maybe a
get offset (OffsetPlane _ f) = f offset


-- disregard : Plane (Offset v) a -> Plane v a
-- disregard = transformBy (\(Offset off) -> off) Offset


foldMap : (Cell Vec2 a -> b) -> OffsetPlane Vec2 a -> List (List b)
foldMap f (OffsetPlane limits planeF as plane) =
    rect limits
        |> List.map (List.map <| \v -> f (v, planeF <| Offset v))


materialize : OffsetPlane Vec2 a -> List (List (Cell Vec2 a))
materialize = foldMap identity


materializeFlatten : OffsetPlane Vec2 a -> List (Cell Vec2 a)
materializeFlatten = materialize >> List.concat


materializeExists : OffsetPlane Vec2 a -> List (Vec2, a)
materializeExists =
    materializeFlatten
        >> List.map cellToMaybe
        >> List.filterMap identity


toOffset : v -> Offset v
toOffset = Offset


toDirection : Offset Vec2 -> Direction
toDirection (Offset (x, y)) =
    if (x < 0) && (y < 0) then NE
    else if (x == 0) && (y < 0) then N
    else if (x > 0) && (y < 0) then NW
    else if (x < 0) && (y == 0) then E
    else if (x == 0) && (y == 0) then X
    else if (x > 0) && (y == 0) then W
    else if (x < 0) && (y > 0) then SE
    else if (x == 0) && (y > 0) then S
    else SW


fromDirection : Direction -> Offset Vec2
fromDirection direction =
    Offset <| case direction of
        NE -> ( -1, -1 )
        N  -> (  0, -1 )
        NW -> (  1, -1 )
        E  -> ( -1,  0 )
        X  -> (  0,  0 )
        W  -> (  1,  0 )
        SE -> ( -1,  1 )
        S  -> (  0,  1 )
        SW -> (  1,  1 )

