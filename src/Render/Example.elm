module Render.Example exposing (..)

import Html exposing (Html)

import WFC.Plane exposing (Plane, N)
import WFC.Plane.Flat exposing (SearchMethod)
import WFC.Plane.Impl.Tracing exposing (TracingPlane)
import WFC.Solver exposing (Step)
import WFC.Solver.History exposing (History)


type alias Renderer v fmt a msg =
    { source : v -> fmt -> Html msg
    , tracing : TracingPlane v a -> Html msg
    , tracingTiny : TracingPlane v a -> Html msg
    , subPlanes : Plane v a -> Html msg
    , periodicSubPlanes : Plane v a -> Html msg
    , allViews : Plane v a -> Html msg
    , rotationsAndFlips : Plane v a -> Html msg
    , materialized : Plane v a -> Html msg
    , patterns : SearchMethod -> N v -> Plane v a -> Html msg
    , allSubPlanes : SearchMethod -> N v -> Plane v a -> Html msg
    , step : Step v -> Html msg
    , history : History (Step v) -> Html msg
    }
