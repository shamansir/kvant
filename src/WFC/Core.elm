module WFC.Core exposing
    ( WFC, Instance(..)
    , TracingWFC
    , text, TextWFC
    , textTracing, TextTracingWFC, TextTracingPlane
    , firstStep
    , run, step, stepAtOnce
    )


import Random


import WFC.Vec2 exposing (..)
import WFC.Plane.Impl.Text as TextPlane exposing (make, toString)
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


type Instance
    = Text (String -> TextWFC) (Step Vec2)
    | TextTracing (String -> TextTracingWFC) (Step Vec2)


type alias TextWFC = WFC Vec2 String Char

type alias TracingWFC v a = WFC v (TracingPlane v a) a
type alias TextTracingWFC = TracingWFC Vec2 Char
type alias TextTracingPlane = TracingPlane Vec2 Char


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



text : Solver.Options Vec2 -> (String -> TextWFC)
text options =
    makeFn
        (Convert
            { fromInput = TextPlane.make options.inputSize
            , toElement = always (List.head >> Maybe.withDefault 'x')
            , toOutput = TextPlane.toString
            }
        )
        (FlatSolver.init options)


textTracing : Solver.Options Vec2 -> (String -> TextTracingWFC)
textTracing options =
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
                    |> TextPlane.make options.inputSize
                    |> FlatSolver.init options
            )
            (Plane.empty options.outputSize)
    -- input
    --     |> TextPlane.make options.inputSize
    --     |> FlatSolver.init options
    --     |> make
    --         (Convert
    --             { fromInput = identity
    --             , toElement = Tuple.pair
    --             , toOutput = identity
    --             }
    --         )
