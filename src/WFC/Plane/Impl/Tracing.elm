module WFC.Plane.Impl.Tracing exposing (..)

import Array


import WFC.Vec2 exposing (..)
import WFC.Plane exposing (..)
import WFC.Plane.Flat exposing (..)
import WFC.Solver exposing (CellState(..))


type alias TracingCell a = (CellState, List a)

type alias TracingPlane v a = Plane v (TracingCell a)


initFrom : Plane v a -> TracingPlane v a
initFrom = map (List.singleton >> Tuple.pair (Entropy 1.0))
