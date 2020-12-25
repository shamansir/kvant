module Example.Render.Renderer exposing (..)

import Html exposing (Html)
import Canvas exposing (Renderable)

import Kvant.Plane exposing (Plane, N)
import Kvant.Plane.Flat exposing (Boundary)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Solver exposing (Step)
import Kvant.Solver.History exposing (History)


type alias HtmlRenderer v fmt a msg = Renderer v fmt a (Html msg)

type alias CanvasRenderer v fmt a = Renderer v fmt a Renderable


type alias Renderer v fmt a target =
    { source : fmt -> target
    , tracing : TracingPlane v a -> target
    , tracingTiny : TracingPlane v a -> target
    , subPlanes : Plane v a -> target
    , periodicSubPlanes : Plane v a -> target
    , allViews : Plane v a -> target
    , rotationsAndFlips : Plane v a -> target
    , materialized : Plane v a -> target
    , patterns : Boundary -> N v -> Plane v a -> target
    , allSubPlanes : Boundary -> N v -> Plane v a -> target
    , step : Step v -> target
    , history : History (Step v) -> target
    }
