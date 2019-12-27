module WFC.Plane.Offset exposing (..)


import Dict
import Dict exposing (Dict)


import WFC.Plane.Plane exposing (..)

import WFC.Vec2 exposing (..)


type Offset v = Offset v


type OffsetPlane v a = OffsetPlane { from: v, to: v } (Offset v -> Maybe a)


-- disregard : Plane (Offset v) a -> Plane v a
-- disregard = transformBy (\(Offset off) -> off) Offset


foldMap : (Cell Vec2 a -> b) -> OffsetPlane Vec2 a -> List (List b)
foldMap f (OffsetPlane limits planeF as plane) =
    rect limits
        |> List.map (List.map <| \v -> f (v, planeF <| Offset v))

