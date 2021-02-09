module Kvant.Core exposing
    ( Wfc
    , make, makeAdvancing
    , run
    , step, stepAtOnce
    , firstStep
    )

import Random

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Size)
import Kvant.Solver as Solver exposing (Step(..), Adjacency, Solution)
import Kvant.Matches exposing (..)
-- import Kvant.Patterns exposing (UniquePatterns)
-- import Kvant.Adjacency exposing (Adjacency)


type Wfc = -- FIXME: Store `Adjacency` inside
    Wfc ( Solver.Step -> Solver.Step )


makeWith : (Adjacency a -> Step -> Step) -> Adjacency a -> Wfc
makeWith doStep =
    {- let
        uniquePatterns =
            case options.approach of
                PatternSearch o ->
                    source
                        |> Patterns.preprocess o.symmetry o.inputBoundary o.patternSize
                Tiled ->
                    Dict.empty
    in -}
    Wfc << doStep


make : Adjacency a -> Wfc
make = makeWith Solver.solve


makeAdvancing : Adjacency a -> Wfc
makeAdvancing = makeWith Solver.advance


-- FIXME: do not require adjacency for running
run : Adjacency a -> Random.Seed -> Size -> Wfc -> Solution a
run adjacency seed size (Wfc doStep as wfc) =
    -- FIXME: we do two steps actually, because with the first step the `Solver` inits the wave
    --        (see `Initial` status) and proceeds with solving only after that
    doStep (wfc |> firstStep seed size)
        |> Solver.produce adjacency


step : Step -> Wfc -> Step
step stepToPerform (Wfc wfc) = wfc stepToPerform


stepAtOnce : List Step -> Wfc -> Maybe Step
stepAtOnce steps wfc =
    steps
        |> List.foldl
            (\nextStep _ ->
                wfc |> step nextStep |> Just
            )
            Nothing


firstStep : Random.Seed -> Size -> Wfc -> Step
firstStep seed size =
    step <| Solver.firstStep size seed
-- FIXME: do not perform, just return?
