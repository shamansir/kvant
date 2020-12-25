module Example.Instance.Image exposing (..)


import Color exposing (Color)

import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Example.Advance exposing (Status(..), AdvanceMode(..))
import Example.Example as Example exposing (Example, make)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane as Plane exposing (empty)
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)
import Kvant.Solver.Flat as FlatSolver exposing (init)

import Example.Instance.Image.Plane as ImagePlane exposing (make)


type alias ImageExample = Example Vec2 Image Color


type alias ImageWfc = Wfc Vec2 Image Color
type alias ImageTracingWfc = TracingWfc Vec2 Color
type alias ImageTracingPlane = TracingPlane Vec2 Color


type alias ImageOptions = Solver.Options Vec2 Color


quick : Solver.Options Vec2 Color -> Image -> ImageExample
quick options source =
    let
        { width, height } = Image.dimensions source
    in
        Example.make
            (\advanceMode ->
                ( case advanceMode of
                    AtOnce -> image options source
                    StepByStep -> imageAdvancing options source
                , imageTracing options source
                )
            )
            options
            source
            (ImageC.toArray2d source
                |> ImagePlane.makeInBounds ( width, height ))


image : ImageOptions -> (Image -> ImageWfc)
image options =
    makeFn
        (Convert
            { fromInput = ImagePlane.fromImage
            , toElement = always ImagePlane.merge
            , toOutput = ImagePlane.toImage
            }
        )
        (FlatSolver.init options)


imageAdvancing  : ImageOptions -> (Image -> ImageWfc)
imageAdvancing options =
    makeAdvancingFn
        (Convert
            { fromInput = ImagePlane.fromImage
            , toElement = always ImagePlane.merge
            , toOutput = ImagePlane.toImage
            }
        )
        (FlatSolver.init options)


imageTracing : ImageOptions -> (Image -> ImageTracingWfc)
imageTracing options =
    \input ->
        makeAdvancingFn
            (Convert
                { fromInput = identity
                , toElement = Tuple.pair
                , toOutput = identity
                }
            )
            (\_ ->
                input
                    |> ImagePlane.fromImage
                    |> FlatSolver.init options
            )
            (Plane.empty options.outputSize)
