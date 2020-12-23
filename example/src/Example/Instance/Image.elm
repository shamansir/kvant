module Example.Instance.Image exposing (..)


import Color exposing (Color)

import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Example.Advance exposing (Status(..), AdvanceMode(..))
import Example.Example as Example exposing (Example, make)
import Example.Instance exposing (image, imageAdvancing, imageTracing)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)

import Example.Instance.Image.Plane as ImagePlane exposing (make)


type alias ImageExample = Example Vec2 Image Color


quick : Solver.Options Vec2 Color -> Image -> ImageExample
quick options image =
    let
        { width, height } = Image.dimensions image
    in
        Example.make
            (\advanceMode ->
                ( case advanceMode of
                    AtOnce -> Kvant.image options image
                    StepByStep -> Kvant.imageAdvancing options image
                , Kvant.imageTracing options image
                )
            )
            options
            image
            (ImageC.toArray2d image
                |> ImagePlane.makeInBounds ( width, height ))
