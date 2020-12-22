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

import Example.Plane.Impl.Text as TextPlane exposing (make, toString, merge)
import Example.Plane.Impl.Image as ImagePlane exposing (makeInBounds, merge)
import Example.Plane.Impl.Image exposing (Pixels, makeInBounds, merge)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane, TracingCell)
import Kvant.Plane.Impl.Tracing as TracingPlane exposing (..)


type Instance
    = Text TextWfc
    | TextTracing TextTracingWfc
    | Image ImageWfc
    | ImageTracing ImageTracingWfc


type alias TextWfc = Wfc Vec2 BoundedString Char
type alias TextTracingWfc = TracingWfc Vec2 Char
type alias TextTracingPlane = TracingPlane Vec2 Char


type alias ImageWfc = Wfc Vec2 Image Color
type alias ImageTracingWfc = TracingWfc Vec2 Color
type alias ImageTracingPlane = TracingPlane Vec2 Color


type alias PixelsWfc = Wfc Vec2 Pixels Color
type alias PixelsTracingWfc = TracingWfc Vec2 Color
type alias PixelsTracingPlane = TracingPlane Vec2 Color


type alias TextOptions = Solver.Options Vec2 Char
type alias ImageOptions = Solver.Options Vec2 Color
type alias PixelsOptions = Solver.Options Vec2 Color



-- FIXME: Simplify `makeFn` and `makeAdvancingFn`.
--        Find the way to


makeFn : Converter v a x fmt -> (Plane v x -> Solver v a) -> (fmt -> Wfc v fmt a)
makeFn (Convert convert as cnv) initSolver input =
    input
        |> convert.fromInput
        |> initSolver
        |> C.make cnv


makeAdvancingFn : Converter v a x fmt -> (Plane v x -> Solver v a) -> (fmt -> Wfc v fmt a)
makeAdvancingFn (Convert convert as cnv) initSolver input =
    input
        |> convert.fromInput
        |> initSolver
        |> C.makeAdvancing cnv


text : TextOptions -> (BoundedString -> TextWfc)
text options =
    makeFn
        (Convert
            { fromInput = \(size, str) -> TextPlane.make size str
            , toElement = always TextPlane.merge
            , toOutput = TextPlane.toBoundedString
            }
        )
        (FlatSolver.init options)


textAdvancing  : TextOptions -> (BoundedString -> TextWfc)
textAdvancing options =
    makeAdvancingFn
        (Convert
            { fromInput = \(size, str) -> TextPlane.make size str
            , toElement = always TextPlane.merge
            , toOutput = TextPlane.toBoundedString
            }
        )
        (FlatSolver.init options)


textTracing : TextOptions -> (BoundedString -> TextTracingWfc)
textTracing options =
    \(size, input) ->
        makeAdvancingFn
            (Convert
                { fromInput = identity
                , toElement = Tuple.pair
                , toOutput = identity
                }
            )
            (\_ ->
                input
                    |> TextPlane.make size
                    |> FlatSolver.init options
            )
            (Plane.empty options.outputSize)



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
