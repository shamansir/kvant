module Render.Example.Image exposing (..)


import Color exposing (Color)

import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Render.Example exposing (ImageExample, Status(..), AdvanceMode(..))
import Render.Example as Example exposing (make)

import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Cell, N(..))
import WFC.Plane.Flat exposing (Boundary(..), Symmetry(..))
import WFC.Core as WFC exposing (..)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step(..), Options)
import WFC.Plane.Impl.Image as ImagePlane exposing (make)


quick : WFC.Options Vec2 Color -> Image -> ImageExample
quick options image =
    let
        { width, height } = Image.dimensions image
    in
        Example.make
            (\advanceMode ->
                ( case advanceMode of
                    AtOnce -> WFC.image options image
                    StepByStep -> WFC.imageAdvancing options image
                , WFC.imageTracing options image
                )
            )
            options
            image
            (ImageC.toArray2d image
                |> ImagePlane.makeInBounds ( width, height ))
