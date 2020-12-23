module Example.Render.Renderer exposing (..)

import Html exposing (Html)

import Kvant.Plane exposing (Plane, N)
import Kvant.Plane.Flat exposing (Boundary)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Solver exposing (Step)
import Kvant.Solver.History exposing (History)


type alias Renderer v fmt a msg =
    { source : fmt -> Html msg
    , tracing : TracingPlane v a -> Html msg
    , tracingTiny : TracingPlane v a -> Html msg
    , subPlanes : Plane v a -> Html msg
    , periodicSubPlanes : Plane v a -> Html msg
    , allViews : Plane v a -> Html msg
    , rotationsAndFlips : Plane v a -> Html msg
    , materialized : Plane v a -> Html msg
    , patterns : Boundary -> N v -> Plane v a -> Html msg
    , allSubPlanes : Boundary -> N v -> Plane v a -> Html msg
    , step : Step v -> Html msg
    , history : History (Step v) -> Html msg
    }
