module Example.Render exposing (..)


import Array

import Html exposing (..)
import Html.Attributes exposing (..)

import Kvant.Occurrence exposing (..)
import Kvant.Solver.History exposing (History)
import Kvant.Solver.History as History
import Kvant.Solver exposing (Step(..), StepStatus(..), FocusState(..))
import Kvant.Plane exposing (..)
import Kvant.Plane.Impl.Tracing exposing (..)
import Kvant.Matches as Matches exposing (..)


type alias Renderer v fmt a target =
    { source : fmt -> target
    , plane : Plane v a -> target
    , tracingPlane : TracingPlane v a -> target
    , tracingCell : TracingCell a -> target
    }
