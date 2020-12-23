module Example.Render.Block exposing (..)

import Example.Advance exposing (..)

import Kvant.Plane exposing (Plane, N)
import Kvant.Plane.Flat exposing (Boundary)


type Block v fmt a
    = Source fmt
    | RunOnce v (Status v fmt a)
    | Tracing (Status v fmt a)
    | RotationsAndFlips (Plane v a)
    | SubPlanes (Plane v a)
    | PeriodicSubPlanes (Plane v a)
    | AllViews (Plane v a)
    | Patterns (Plane v a) Boundary (N v)
    | AllSubPlanes (Plane v a) Boundary (N v)
    | Empty


type BlockState
    = Expanded
    | Collapsed


title : Block v fmt a -> String
title block =
    case block of
        Source _ -> "Source"
        RunOnce _ _ -> "Run"
        Tracing _ -> "Trace"
        RotationsAndFlips _ -> "Rotations and Flips"
        SubPlanes _ -> "SubPlanes"
        PeriodicSubPlanes _ -> "Periodic SubPlanes"
        AllViews _ -> "Views"
        Patterns _ _ _ -> "Patterns"
        AllSubPlanes _ _ _ -> "All Possible SubPlanes"
        Empty -> "?"


switchBlock : Int -> List BlockState -> List BlockState
switchBlock index states =
    states
        |> List.indexedMap
            (\blockIndex expandState ->
                if index == blockIndex then
                    case expandState of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                else expandState
            )
