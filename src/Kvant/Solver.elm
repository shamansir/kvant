module Kvant.Solver exposing (..)


import Dict
import Dict exposing (Dict)
import Random

import Kvant.Vec2 as Vec2
import Kvant.Matches exposing (Matches)
import Kvant.Matches as Matches
import Kvant.Plane exposing (Plane(..), Coord, Size)
import Kvant.Plane as Plane
import Kvant.Patterns exposing (UniquePatterns)
import Kvant.Neighbours as Neighbours
import Kvant.Neighbours as Dir exposing (Direction(..))
import Kvant.Adjacency as A


type alias Adjacency a = A.Adjacency Int a


type alias AtomId = Int


type alias Weight = Float


type alias Wave = Plane (Matches AtomId)


type alias Solution = Plane (List AtomId)


type Step
    = Step Int Random.Seed Size StepStatus


type StepStatus
    = Initial
    | InProgress FocusState Wave
    | Solved Wave
    | Terminated -- terminated by contradiction
    | ReachedLimit Int


type FocusState
    = NotFocused
    | FocusedAt Coord


type MaximumSteps = MaximumSteps Int


type Observation
    = Collapsed
    | Contradiction
    | Focus Coord AtomId


firstStep : Size -> Random.Seed -> Step
firstStep size seed =
    Step 0 seed size Initial


solve : Adjacency a -> Step -> Step
solve adjacencyRules fromStep =
    let
        succeedingStep = advance adjacencyRules fromStep
    in case getStatus succeedingStep of
        Solved _ -> succeedingStep
        Terminated -> succeedingStep
        _ -> solve adjacencyRules succeedingStep


solveUntil : MaximumSteps -> Adjacency a -> Step -> Step
solveUntil (MaximumSteps maxSteps) adjacencyRules fromStep =
    let
        succeedingStep = advance adjacencyRules fromStep
    in case getStatus succeedingStep of
        Solved _ -> succeedingStep
        Terminated -> succeedingStep
        _ ->
            if not (succeedingStep |> exceeds maxSteps)
                then succeedingStep |> solve adjacencyRules
                else succeedingStep |> (updateStatus <| ReachedLimit maxSteps)


advance : Adjacency a -> Step -> Step
advance adjacencyRules (Step _ _ outputSize _ as step) =
    case getStatus step of
        Initial ->
            initWave (Dict.keys adjacencyRules) outputSize
                |> InProgress NotFocused
                |> nextStep (getSeed step) step
        InProgress _ wave ->
            case observe (getSeed step) adjacencyRules wave of
                ( Collapsed, oSeed ) ->
                    nextStep oSeed step <| Solved wave
                ( Contradiction, oSeed ) ->
                    nextStep oSeed step <| Terminated
                ( Focus position atom, oSeed ) ->
                    case wave
                            |> propagate adjacencyRules position atom of
                        newWave ->
                            nextStep oSeed step <| InProgress (FocusedAt position) newWave
        _ -> step


observe
    :  Random.Seed
    -> Adjacency a
    -> Wave
    -> ( Observation, Random.Seed )
observe seed adjacencyRules wave =
    if wave |> hasAContradiction then
        ( Contradiction, seed )
    else if wave |> isCollapsed then
        ( Collapsed, seed )
    else
        let
            ( result, eSeed ) =
                wave |> findLowestEntropy seed adjacencyRules
            ( coord, cSeed ) =
                case result of
                    Just c -> ( c, eSeed )
                    Nothing ->
                        Random.step (Vec2.random <| Plane.size wave) eSeed
        in
            Plane.get coord wave
                |> Maybe.andThen
                    (
                        Matches.run
                            (always Nothing)
                            (\first tail ->
                                let
                                    atomChoiceGenerator =
                                        randomAtom adjacencyRules first tail
                                in
                                    Random.step atomChoiceGenerator cSeed
                                        |> Tuple.mapFirst (Focus coord)
                                        |> Just
                            )
                    )
                |> Maybe.withDefault ( Contradiction, cSeed )


propagate
    :  Adjacency a
    -> Coord
    -> AtomId
    -> Wave
    -> Wave
propagate adjacencyRules focus atom wave =
    let
        probe : Coord -> Matches AtomId -> Wave -> Wave
        probe atPos newMatches prevWave =
            let

                curMatches =
                    prevWave
                        |> Plane.get atPos
                        |> Maybe.withDefault Matches.none

                probeNeighbours withWave =
                    Neighbours.cardinal
                        |> List.foldl
                            (\dir curWave ->

                                case dir |> Dir.move atPos of
                                    moved ->
                                        -- FIXME: support unbounded planes
                                        if not <| Plane.fits moved curWave then curWave
                                        else
                                            let
                                                curMatchesAtDir =
                                                    curWave
                                                        |> Plane.get moved
                                                        |> Maybe.withDefault Matches.none
                                                newMatchesAtDir =
                                                    newMatches
                                                        |> matchesAtDir adjacencyRules dir
                                                        |> Matches.and curMatchesAtDir
                                            in
                                                curWave |> probe moved newMatchesAtDir

                            )
                            withWave

            in
                if Matches.equal curMatches newMatches
                    then prevWave
                    else
                        prevWave
                            |> Plane.set atPos (Matches.and curMatches newMatches)
                            |> probeNeighbours
    in
        wave
            |> probe focus (Matches.single atom)


initWave : List AtomId -> Size -> Wave
initWave items size =
    Plane.filled size <| Matches.fromList items


produce
    :  UniquePatterns
    -> Step
    -> Solution
{-  TODO :  Set a
    -> (a -> b)
    -> Step
    -> Plane (List b)
-}
produce adjacencyRules (Step _ _ outputSize status) =
    let
        loadValues : Matches AtomId -> List AtomId
        loadValues matches =
            matches
                |> Matches.toList
                |> List.map (\atomId ->
                    adjacencyRules
                        |> Dict.get atomId
                        |> Maybe.andThen (.subject >> Plane.get (0, 0))
                    )
                |> List.filterMap identity
                -- if pattern wasn't found or contains no value at this point, it is skipped
        fromWave : Wave -> Plane (List AtomId)
        fromWave wave = wave |> Plane.map loadValues
    in
        fromWave <| case status of
            Initial -> initWave (Dict.keys adjacencyRules) outputSize
            InProgress _ wave -> wave
            Solved wave -> wave
            Terminated -> Plane.empty outputSize
            ReachedLimit _ -> Plane.empty outputSize


noiseCoefficient : Float
noiseCoefficient = 0.1


entropyOf : Random.Seed -> Adjacency a -> Matches AtomId -> ( Maybe Float, Random.Seed )
entropyOf seed adjacencyRules matches =
    case Matches.count matches of
        0 -> ( Nothing, seed ) -- contradiction
        1 -> ( Just 0, seed )
        count ->
            let
                atomWeight : AtomId -> Weight
                atomWeight atomId =
                    Dict.get atomId adjacencyRules |>
                        Maybe.map .weight |>
                        Maybe.withDefault 0
                -- TODO: prepare frequency lists in advance, before calculation
                weights =
                    matches
                        |> Matches.toList
                        |> List.map atomWeight
                maxWeight =
                    List.maximum weights |> Maybe.withDefault 0
                sumOfWeights = List.foldl (+) 0 weights
                sumOfLoggedWeights =
                    weights
                        |> List.map (logBase 2)
                        |> List.foldl (+) 0
                pureEntropy =
                    (logBase 2 sumOfWeights) - (sumOfLoggedWeights / sumOfWeights)
            in
                Random.step (Random.float 0 <| maxWeight * noiseCoefficient) seed
                    |> Tuple.mapFirst ((+) pureEntropy >> Just)


findLowestEntropy
    :  Random.Seed
    -> Adjacency a
    -> Wave
    -> ( Maybe Coord, Random.Seed )
findLowestEntropy seed adjacencyRules =
    let
        withEntropy prevSeed matches f =
            matches
                |> entropyOf prevSeed adjacencyRules
                |> Tuple.mapFirst (Maybe.andThen f)
        foldingF ( curCoord, matches ) ( maybePrevLowest, prevSeed ) =
            case maybePrevLowest of
                Nothing ->
                    withEntropy prevSeed matches
                        <| \curEntropy ->
                            if curEntropy > 0
                            then Just ( curCoord, curEntropy )
                            else maybePrevLowest
                Just ( _, prevMinEntropy ) ->
                    withEntropy prevSeed matches
                        <| \curEntropy ->
                            if curEntropy > 0 && curEntropy < prevMinEntropy
                            then Just ( curCoord, curEntropy )
                            else maybePrevLowest
    in

        Plane.toList
            >> List.foldl
                foldingF
                ( Nothing, seed )
            >> Tuple.mapFirst (Maybe.map Tuple.first)


randomAtom : Adjacency a -> AtomId -> List AtomId -> Random.Generator AtomId
randomAtom adjacencyRules first others =
    let
        packWithWeight : AtomId -> ( Weight, AtomId )
        packWithWeight atomId =
            ( adjacencyRules
                |> Dict.get atomId
                |> Maybe.map .weight
                |> Maybe.withDefault 0
            , atomId
            )
    in
        Random.weighted
            (packWithWeight first)
            (others |> List.map packWithWeight)


matchesAtDir : Adjacency a -> Direction -> Matches AtomId -> Matches AtomId
matchesAtDir adjacencyRules dir matchesAtFocus =
    matchesAtFocus
        |> Matches.toList
        |> List.map (getMatchesOf adjacencyRules dir)
        |> List.map (Maybe.withDefault Matches.none)
        |> Matches.union


getMatchesOf : Adjacency a -> Direction -> AtomId -> Maybe (Matches AtomId)
getMatchesOf adjacencyRules dir atom =
    adjacencyRules
        |> Dict.get atom
        |> Maybe.andThen
            (\{ matches } ->
                matches |> Dict.get (Dir.offsetFor dir)
            )


hasAContradiction : Wave -> Bool
hasAContradiction =
    Plane.foldl
        (\matches wasAContradiction ->
            wasAContradiction || Matches.isNone matches
        )
        False


isCollapsed : Wave -> Bool
isCollapsed =
    Plane.foldl
        (\matches wasCollapsed ->
            wasCollapsed && (Matches.count matches == 1)
        )
        True


getSeed : Step -> Random.Seed
getSeed (Step _ seed _ _) = seed


getStatus : Step -> StepStatus
getStatus (Step _ _ _ status) = status


nextStep : Random.Seed -> Step -> StepStatus -> Step
nextStep seed (Step n _ options _) status = Step (n + 1) seed options status


updateStatus : StepStatus -> Step -> Step
updateStatus status (Step n seed opts _) = Step n seed opts status


exceeds : Int -> Step -> Bool
exceeds count (Step stepN _ _ _) = count <= stepN


extractMatchesAt : Coord -> Step -> Maybe (Dir.Neighbours (Matches AtomId))
extractMatchesAt coord (Step _ _ _ status) =
    case status of
        Initial -> Nothing
        Terminated -> Nothing
        ReachedLimit _ -> Nothing
        InProgress _ wave ->
            wave |> Plane.getNeighboursOr Matches.none coord |> Just
        Solved wave ->
            wave |> Plane.getNeighboursOr Matches.none coord |> Just

