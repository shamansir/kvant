module Example.Pixels exposing (..)


import Array
import Color exposing (Color)

import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Example.Main exposing (PixelsExample, Status(..), AdvanceMode(..))
import Example.Main as Example exposing (make)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)
import Kvant.Plane.Impl.Image exposing (Pixels)
import Kvant.Plane.Impl.Image as ImagePlane exposing (make)


quick : Solver.Options Vec2 Color -> Pixels -> PixelsExample
quick options pixels =
    let
        ( width, height ) =
            ( Array.get 0 pixels
                |> Maybe.map Array.length
                |> Maybe.withDefault 0
            , Array.length pixels
            )
    in
        Example.make
            (\advanceMode ->
                ( case advanceMode of
                    AtOnce -> Kvant.pixels options pixels
                    StepByStep -> Kvant.pixelsAdvancing options pixels
                , Kvant.pixelsTracing options pixels
                )
            )
            options
            pixels
            (pixels |> ImagePlane.makeInBounds ( width, height ))
