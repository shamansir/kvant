module Kvant.Core exposing
    ( Wfc, Instance(..)
    , TracingWfc
    , text, textAdvancing, TextWfc, TextOptions
    , textTracing, TextTracingWfc, TextTracingPlane
    , image, imageAdvancing, ImageWfc, ImageOptions
    , imageTracing, ImageTracingWfc, ImageTracingPlane
    , firstStep
    , run, step, stepAtOnce
    , BoundedString
    )


import Random
import Image exposing (Image)
import Image as Image exposing (..)
import Color exposing (Color)


import Kvant.Vec2 exposing (..)
import Kvant.Plane.Impl.Text as TextPlane exposing (make, toString, merge)
import Kvant.Plane.Impl.Image as ImagePlane exposing (makeInBounds, merge)
import Kvant.Plane.Impl.Image exposing (Pixels, makeInBounds, merge)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane, TracingCell)
import Kvant.Plane.Impl.Tracing as TracingPlane exposing (..)
import Kvant.Plane exposing (Plane)
import Kvant.Plane as Plane exposing (empty)
import Kvant.Solver exposing (Solver)
import Kvant.Solver as Solver exposing (Step(..), getSource)
import Kvant.Solver.Flat as FlatSolver exposing (init)
import Kvant.Matches exposing (..)


type Wfc v fmt a =
    Wfc ( Solver.Step v -> ( Solver.Step v, fmt ) )

type alias TracingWfc v a = Wfc v (TracingPlane v a) a


-- type Instance
--     = Text (String -> TextWfc) (Step Vec2)
--     | TextTracing (String -> TextTracingWfc) (Step Vec2)


type Instance
    = Text TextWfc
    | TextTracing TextTracingWfc
    | Image ImageWfc
    | ImageTracing ImageTracingWfc


type alias BoundedString = (Vec2, String)


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


type Converter v a x fmt =
    Convert
        { fromInput : fmt -> Plane v x
        , toElement : Matches Solver.PatternId -> List a -> x
        , toOutput : Plane v x -> fmt
        }


make : Converter v a x fmt -> Solver v a -> Wfc v fmt a
make (Convert convert) solver =
    Wfc <|
        \nextStep ->
            let
                lastStep = Solver.solve solver nextStep
            in
                ( lastStep
                , lastStep
                    |> Solver.apply convert.toElement solver
                    |> convert.toOutput
                )


makeAdvancing : Converter v a x fmt -> Solver v a -> Wfc v fmt a
makeAdvancing (Convert convert) solver =
    Wfc <|
        \nextStep ->
            let
                lastStep = Solver.advance solver nextStep
            in
                ( lastStep
                , lastStep
                    |> Solver.apply convert.toElement solver
                    |> convert.toOutput
                )


run : Random.Seed -> Wfc v fmt a -> fmt
run seed wfc =
    -- FIXME: we do two steps actually, because with the first step the `Solver` inits the wave
    --        (see `Initial` status) and proceeds with solving only after that
    step (firstStep seed wfc |> Tuple.first) wfc
        |> Tuple.second


step : Step v -> Wfc v fmt a -> ( Step v, fmt )
step stepToPerform (Wfc wfc) = wfc stepToPerform


stepAtOnce : List (Step v) -> Wfc v fmt a -> Maybe ( Step v, fmt )
stepAtOnce steps wfc =
    steps
        |> List.foldl
            (\nextStep _ ->
                wfc |> step nextStep |> Just
            )
            Nothing


firstStep : Random.Seed -> Wfc v fmt a -> ( Step v, fmt )
firstStep = step << Solver.firstStep


-- FIXME: Simplify `makeFn` and `makeAdvancingFn`.
--        Find the way to


makeFn : Converter v a x fmt -> (Plane v x -> Solver v a) -> (fmt -> Wfc v fmt a)
makeFn (Convert convert as cnv) initSolver input =
    input
        |> convert.fromInput
        |> initSolver
        |> make cnv


makeAdvancingFn : Converter v a x fmt -> (Plane v x -> Solver v a) -> (fmt -> Wfc v fmt a)
makeAdvancingFn (Convert convert as cnv) initSolver input =
    input
        |> convert.fromInput
        |> initSolver
        |> makeAdvancing cnv


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


pixels : ImageOptions -> (Pixels -> PixelsWfc)
pixels options =
    makeFn
        (Convert
            { fromInput = ImagePlane.make
            , toElement = always ImagePlane.merge
            , toOutput = ImagePlane.toPixels
            }
        )
        (FlatSolver.init options)


pixelsAdvancing  : ImageOptions -> (Pixels -> PixelsWfc)
pixelsAdvancing options =
    makeAdvancingFn
        (Convert
            { fromInput = ImagePlane.make
            , toElement = always ImagePlane.merge
            , toOutput = ImagePlane.toPixels
            }
        )
        (FlatSolver.init options)


pixelsTracing : ImageOptions -> (Pixels -> PixelsTracingWfc)
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
