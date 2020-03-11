module Kvant.Plane.Impl.Tracing exposing (..)

import Array


import Kvant.Vec2 exposing (..)
import Kvant.Matches exposing (..)
import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)
import Kvant.Solver exposing (PatternId)


-- not Impl, should be on the same level as Flat

type alias TracingCell a = (Matches PatternId, List a)

type alias TracingPlane v a = Plane v (TracingCell a)


initFrom : Plane v a -> TracingPlane v a
initFrom = map (List.singleton >> Tuple.pair none)
