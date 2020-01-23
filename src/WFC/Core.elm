module WFC.Core exposing
    ( WFC, Instance(..)
    , TracingWFC
    , text, TextWFC
    , textTracing, TextTracingWFC, TextTracingPlane
    , image, ImageWFC
    , imageTracing, ImageTracingWFC, ImageTracingPlane
    , firstStep
    , run, step, stepAtOnce
    , BoundedString
    )


import Random
import Image exposing (Image)
import Image as Image exposing (..)
import Color exposing (Color)


import WFC.Vec2 exposing (..)
import WFC.Plane.Impl.Text as TextPlane exposing (make, toString, merge)
import WFC.Plane.Impl.Image as ImagePlane exposing (makeInBounds, merge)
import WFC.Plane.Impl.Tracing exposing (TracingPlane, TracingCell)
import WFC.Plane.Impl.Tracing as TracingPlane exposing (..)
import WFC.Plane exposing (Plane)
import WFC.Plane as Plane exposing (empty)
import WFC.Solver exposing (Solver)
import WFC.Solver as Solver exposing (Step(..), getSource)
import WFC.Solver.Flat as FlatSolver exposing (init)
import WFC.Matches exposing (..)


type WFC v fmt a =
    WFC (Solver v a) ( Solver.Step v -> ( Solver.Step v, fmt ) )

type alias TracingWFC v a = WFC v (TracingPlane v a) a


-- type Instance
--     = Text (String -> TextWFC) (Step Vec2)
--     | TextTracing (String -> TextTracingWFC) (Step Vec2)


type Instance
    = Text TextWFC
    | TextTracing TextTracingWFC
    | Image ImageWFC
    | ImageTracing ImageTracingWFC


type alias BoundedString = (Vec2, String)


type alias TextWFC = WFC Vec2 BoundedString Char
type alias TextTracingWFC = TracingWFC Vec2 Char
type alias TextTracingPlane = TracingPlane Vec2 Char


type alias ImageWFC = WFC Vec2 Image Color
type alias ImageTracingWFC = TracingWFC Vec2 Color
type alias ImageTracingPlane = TracingPlane Vec2 Color


type Converter v a x fmt =
    Convert
        { fromInput : fmt -> Plane v x
        , toElement : Matches Solver.PatternId -> List a -> x
        , toOutput : Plane v x -> fmt
        }


make : Converter v a x fmt -> Solver v a -> WFC v fmt a
make (Convert convert) solver =
    WFC solver <|
        \nextStep ->
            let
                lastStep = Solver.solve solver nextStep
            in
                ( lastStep
                , lastStep
                    |> Solver.apply convert.toElement solver
                    |> convert.toOutput
                )


run : Random.Seed -> WFC v fmt a -> fmt
run seed = firstStep seed >> Tuple.second


step : Step v -> WFC v fmt a -> ( Step v, fmt )
step stepToPerform (WFC _ wfc) = wfc stepToPerform


stepAtOnce : List (Step v) -> WFC v fmt a -> Maybe ( Step v, fmt )
stepAtOnce steps wfc =
    steps
        |> List.foldl
            (\nextStep _ ->
                wfc |> step nextStep |> Just
            )
            Nothing


firstStep : Random.Seed -> WFC v fmt a -> ( Step v, fmt )
firstStep = step << Solver.firstStep


makeFn : Converter v a x fmt -> (Plane v x -> Solver v a) -> (fmt -> WFC v fmt a)
makeFn (Convert convert as cnv) initSolver input =
    input
        |> convert.fromInput
        |> initSolver
        |> make cnv


text : Solver.Options Vec2 -> (BoundedString -> TextWFC)
text options =
    makeFn
        (Convert
            { fromInput = \(size, str) -> TextPlane.make size str
            , toElement = always TextPlane.merge
            , toOutput = TextPlane.toBoundedString
            }
        )
        (FlatSolver.init options)


textTracing : Solver.Options Vec2 -> (BoundedString -> TextTracingWFC)
textTracing options =
    \(size, input) ->
        makeFn
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



image : Solver.Options Vec2 -> (Image -> ImageWFC)
image options =
    makeFn
        (Convert
            { fromInput = ImagePlane.fromImage
            , toElement = always ImagePlane.merge
            , toOutput = ImagePlane.toImage
            }
        )
        (FlatSolver.init options)


imageTracing : Solver.Options Vec2 -> (Image -> ImageTracingWFC)
imageTracing options =
    \input ->
        makeFn
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
