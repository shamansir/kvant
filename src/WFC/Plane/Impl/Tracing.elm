module WFC.Plane.Impl.Tracing exposing (..)

import Array


import WFC.Vec2 exposing (..)
import WFC.Matches exposing (..)
import WFC.Plane exposing (..)
import WFC.Plane.Flat exposing (..)
import WFC.Solver exposing (PatternId)


-- not Impl, should be on the same level as Flat

type alias TracingCell a = (Matches PatternId, List a)

type alias TracingPlane v a = Plane v (TracingCell a)


initFrom : Plane v a -> TracingPlane v a
initFrom = map (List.singleton >> Tuple.pair none)
