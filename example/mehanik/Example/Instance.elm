module Example.Instance exposing (..)

import Color exposing (Color)
import Image exposing (Image)
import Kvant.Vec2 exposing (Vec2)

import Kvant.Core as C exposing (..)
import Kvant.Solver exposing (Solver)
import Kvant.Solver as Solver exposing (Options)

import Kvant.Plane exposing (Plane)
import Kvant.Plane as Plane exposing (empty)
import Kvant.Solver exposing (Solver)
import Kvant.Solver as Solver exposing (Step(..), getSource)
import Kvant.Solver.Flat as FlatSolver exposing (init)
import Kvant.Matches exposing (..)

import Kvant.Plane.Impl.Tracing exposing (TracingPlane, TracingCell)

import Example.Instance.Text exposing (TextWfc, TextTracingWfc)
import Example.Instance.Image  exposing (ImageWfc, ImageTracingWfc)

type Instance
    = Text TextWfc
    | TextTracing TextTracingWfc
    | Image ImageWfc
    | ImageTracing ImageTracingWfc




