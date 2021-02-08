module Kvant.Core exposing
    ( Wfc
    , make, makeAdvancing
    , step, stepAtOnce
    , firstStep
    )

import Random

import Dict

import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Size)
import Kvant.Solver as Solver exposing (Step(..), Adjacency)
import Kvant.Matches exposing (..)
-- import Kvant.Patterns exposing (UniquePatterns)
-- import Kvant.Adjacency exposing (Adjacency)


type Wfc =
    Wfc ( Solver.Step -> Solver.Step )


makeWith : (Adjacency a -> Step -> Step) -> Adjacency a -> Wfc
makeWith doStep =
    {- let
        uniquePatterns =
            case options.approach of
                Overlapping o ->
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


{-
run : Random.Seed -> Wfc -> Solution
run seed wfc =
    -- FIXME: we do two steps actually, because with the first step the `Solver` inits the wave
    --        (see `Initial` status) and proceeds with solving only after that
    step (firstStep seed wfc |> Tuple.first) wfc
        |> Tuple.second
-}


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
