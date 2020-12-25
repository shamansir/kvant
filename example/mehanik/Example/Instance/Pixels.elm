module Example.Instance.Pixels exposing (..)


import Array
import Color exposing (Color)

import Image exposing (Image, dimensions)
import Image.Color as ImageC exposing (toArray2d)

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Cell, N(..))
import Kvant.Plane as Plane exposing (empty)
import Kvant.Plane.Flat exposing (Boundary(..), Symmetry(..))
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Core as Kvant exposing (..)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step(..), Options)
import Kvant.Solver.Flat as FlatSolver exposing (init)

import Example.Example exposing (Example)
import Example.Example as Example exposing (make)
import Example.Advance exposing (Status(..), AdvanceMode(..))
import Example.Instance.Image.Plane exposing (Pixels)
import Example.Instance.Image.Plane as ImagePlane exposing (make)


type alias PixelsExample = Example Vec2 Pixels Color


type alias PixelsWfc = Wfc Vec2 Pixels Color
type alias PixelsTracingWfc = TracingWfc Vec2 Color
type alias PixelsTracingPlane = TracingPlane Vec2 Color


type alias PixelsOptions = Solver.Options Vec2 Color


quick : Solver.Options Vec2 Color -> Pixels -> PixelsExample
quick options source =
    let
        ( width, height ) =
            ( Array.get 0 source
                |> Maybe.map Array.length
                |> Maybe.withDefault 0
            , Array.length source
            )
    in
        Example.make
            (\advanceMode ->
                ( case advanceMode of
                    AtOnce -> pixels options source
                    StepByStep -> pixelsAdvancing options source
                , pixelsTracing options source
                )
            )
            options
            source
            (source |> ImagePlane.makeInBounds ( width, height ))


pixels : PixelsOptions -> (Pixels -> PixelsWfc)
pixels options =
    makeFn
        (Convert
            { fromInput = ImagePlane.make
            , toElement = always ImagePlane.merge
            , toOutput = ImagePlane.toPixels
            }
        )
        (FlatSolver.init options)


pixelsAdvancing  : PixelsOptions -> (Pixels -> PixelsWfc)
pixelsAdvancing options =
    makeAdvancingFn
        (Convert
            { fromInput = ImagePlane.make
            , toElement = always ImagePlane.merge
            , toOutput = ImagePlane.toPixels
            }
        )
        (FlatSolver.init options)


pixelsTracing : PixelsOptions -> (Pixels -> PixelsTracingWfc)
pixelsTracing options =
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
                    |> ImagePlane.make
                    |> FlatSolver.init options
            )
            (Plane.empty options.outputSize)
