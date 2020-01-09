module WFC.Core exposing
    ( WFC, Instance(..)
    , TracingWFC
    , text, TextWFC
    , textTracing, TextTracingWFC, TextTracingPlane
    , run, step, firstStep
    )


import Random


import WFC.Vec2 exposing (..)
import WFC.Plane.Impl.Text as TextPlane exposing (make, toString)
import WFC.Plane.Impl.Tracing exposing (TracingPlane, TracingCell)
import WFC.Plane.Impl.Tracing as TracingPlane exposing (..)
import WFC.Plane exposing (Plane)
import WFC.Solver exposing (Solver)
import WFC.Solver as Solver exposing (Step(..), getSource)
import WFC.Solver.Flat as FlatSolver exposing (init)


type WFC v fmt a =
    WFC (Solver v a) ( Solver.Step v -> ( Solver.Step v, fmt ) )


type Instance
    = Text (String -> TextWFC) (Step Vec2)
    | TextTracing (String -> TextTracingWFC) (Step Vec2)


type alias TextWFC = WFC Vec2 String Char

type alias TracingWFC v a = WFC v (TracingPlane v a) a
type alias TextTracingWFC = TracingWFC Vec2 Char
type alias TextTracingPlane = TracingPlane Vec2 Char


text : Solver.Options Vec2 -> (String -> TextWFC)
text options input =
    input
        |> TextPlane.make options.inputSize
        |> FlatSolver.init options
        |> make TextPlane.toString


textTracing : Solver.Options Vec2 -> (String -> TextTracingWFC)
textTracing options input =
    input
        |> TextPlane.make options.inputSize
        |> FlatSolver.init options
        |> makeTracing 'x'


make : (Plane v a -> fmt) -> Solver v a -> WFC v fmt a
make convert solver =
    WFC solver <|
        \nextStep ->
            let
                lastStep = Solver.solve solver nextStep
            in
                ( lastStep
                , Solver.getSource solver
                    |> Solver.render lastStep
                    |> convert
                )


makeTracing : a -> Solver v a -> TracingWFC v a
makeTracing fallback solver  =
    WFC solver <|
        \nextStep ->
            let
                lastStep = Solver.solve solver nextStep
            in
                ( lastStep
                , lastStep |> Solver.renderTracing fallback solver
                )


run : Random.Seed -> WFC v fmt a -> fmt
run seed = firstStep seed >> Tuple.second


step : Step v -> WFC v fmt a -> ( Step v, fmt )
step stepToPerform (WFC _ wfc) = wfc stepToPerform


firstStep : Random.Seed -> WFC v fmt a -> ( Step v, fmt )
firstStep = step << Solver.firstStep
