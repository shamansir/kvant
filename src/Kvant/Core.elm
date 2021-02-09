module Kvant.Core exposing
    ( Wfc
    , map
    , make, makeAdvancing
    , run
    , step, stepAtOnce
    , firstStep
    )

import Random

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Size)
import Kvant.Solver as Solver exposing (Step(..), Adjacency, Solution)
import Kvant.Adjacency as A
import Kvant.Matches exposing (..)
-- import Kvant.Patterns exposing (UniquePatterns)
-- import Kvant.Adjacency exposing (Adjacency)


type Wfc a =
    Wfc (Adjacency a) ( Solver.Step -> Solver.Step )


map : ( a -> b ) -> Wfc a -> Wfc b
map f (Wfc adjacency doStep) = Wfc (adjacency |> A.map f) doStep


makeWith : (Adjacency a -> Step -> Step) -> Adjacency a -> Wfc a
makeWith doStep adjacency =
    {- let
        uniquePatterns =
            case options.approach of
                PatternSearch o ->
                    source
                        |> Patterns.preprocess o.symmetry o.inputBoundary o.patternSize
                Tiled ->
                    Dict.empty
    in -}
    Wfc adjacency <| doStep adjacency


make : Adjacency a -> Wfc a
make = makeWith Solver.solve


makeAdvancing : Adjacency a -> Wfc a
makeAdvancing = makeWith Solver.advance


run : Random.Seed -> Size -> Wfc a -> Solution a
run seed size (Wfc adjacency doStep as wfc) =
    -- FIXME: we do two steps actually, because with the first step the `Solver` inits the wave
    --        (see `Initial` status) and proceeds with solving only after that
    doStep (wfc |> firstStep seed size)
        |> Solver.produce adjacency


step : Step -> Wfc a -> Step
step stepToPerform (Wfc _ doStep) = doStep stepToPerform


stepAtOnce : List Step -> Wfc a -> Maybe Step
stepAtOnce steps wfc =
    steps
        |> List.foldl
            (\nextStep _ ->
                wfc |> step nextStep |> Just
            )
            Nothing


firstStep : Random.Seed -> Size -> Wfc a -> Step
firstStep seed size =
    step <| Solver.firstStep size seed
-- FIXME: do not perform, just return?
