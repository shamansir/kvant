module Render.Example.Image exposing (..)


import Color exposing (Color)

import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Render.Example exposing (ImageExample, Status(..))
import Render.Example as Example exposing (make)

import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Cell, N(..))
import WFC.Plane.Flat exposing (Boundary(..), Symmetry(..))
import WFC.Core as WFC exposing (..)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step(..), Options)
import WFC.Plane.Impl.Image as ImagePlane exposing (make)


options : WFC.AdvanceRule -> WFC.Options Vec2 Color
options advanceRule =
    { approach =
        Overlapping
            { searchBoundary = Bounded -- Periodic
            , patternSize = N ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    , advanceRule = advanceRule
    }


quick : Image -> ImageExample
quick image =
    let
        { width, height } = Image.dimensions image
    in
        Example.make
            (WFC.image (options <| WFC.MaximumAttempts 200) image)
            (WFC.imageTracing (options WFC.AdvanceManually) image)
            (options WFC.AdvanceManually)
            image
            (ImageC.toArray2d image
                |> ImagePlane.makeInBounds ( width, height ))
