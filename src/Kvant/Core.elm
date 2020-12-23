module Kvant.Core exposing
    ( Wfc, TracingWfc
    , firstStep
    , run, step, stepAtOnce
    , Converter(..), make, makeAdvancing
    , makeFn, makeAdvancingFn
    )


import Random
import Image exposing (Image)
import Image as Image exposing (..)
import Color exposing (Color)


import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Plane)
import Kvant.Plane as Plane exposing (empty)
import Kvant.Solver exposing (Solver)
import Kvant.Solver as Solver exposing (Step(..), getSource)
import Kvant.Solver.Flat as FlatSolver exposing (init)
import Kvant.Matches exposing (..)

import Kvant.Plane.Impl.Tracing exposing (TracingPlane, TracingCell)


type Wfc v fmt a =
    Wfc ( Solver.Step v -> ( Solver.Step v, fmt ) )

type alias TracingWfc v a = Wfc v (TracingPlane v a) a


-- type Instance
--     = Text (String -> TextWfc) (Step Vec2)
--     | TextTracing (String -> TextTracingWfc) (Step Vec2)


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
