module Kvant.Core exposing
    ( Wfc
    , firstStep
    , run, step, stepAtOnce
    , make, makeAdvancing
    )

import Random

import Dict

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Plane, Size)
import Kvant.Solver as Solver exposing (Step(..), Solution)
import Kvant.Solver.Options as Solver exposing (Approach(..))
import Kvant.Patterns as Patterns
import Kvant.Matches exposing (..)
import Kvant.Patterns exposing (UniquePatterns)


type Wfc =
    -- may be `Plane (List Key)` is not needed, just extract it from the wave
    Wfc Size ( Solver.Step -> ( Solver.Step, Solution ) )


makeWith : (UniquePatterns -> Step -> Step) -> Solver.Options -> Plane Patterns.AtomId -> Wfc
makeWith doStep options source =
    let
        uniquePatterns =
            case options.approach of
                Overlapping o ->
                    source
                        |> Patterns.preprocess o.symmetry o.inputBoundary o.patternSize
                Tiled ->
                    Dict.empty
    in
    Wfc options.outputSize <|
        \nextStep ->
            let
                lastStep = doStep uniquePatterns nextStep
            in
                ( lastStep
                , lastStep |> Solver.produce uniquePatterns
                )


make : Solver.Options -> Plane Patterns.AtomId -> Wfc
make = makeWith Solver.solve


makeAdvancing : Solver.Options -> Plane Patterns.AtomId -> Wfc
makeAdvancing = makeWith Solver.advance


run : Random.Seed -> Wfc -> Solution
run seed wfc =
    -- FIXME: we do two steps actually, because with the first step the `Solver` inits the wave
    --        (see `Initial` status) and proceeds with solving only after that
    step (firstStep seed wfc |> Tuple.first) wfc
        |> Tuple.second


step : Step -> Wfc -> ( Step, Solution )
step stepToPerform (Wfc _ wfc) = wfc stepToPerform


stepAtOnce : List Step -> Wfc -> Maybe ( Step, Solution )
stepAtOnce steps wfc =
    steps
        |> List.foldl
            (\nextStep _ ->
                wfc |> step nextStep |> Just
            )
            Nothing


firstStep : Random.Seed -> Wfc -> ( Step, Solution )
firstStep seed (Wfc size _ as wfc) =
    step (Solver.firstStep size seed) wfc
