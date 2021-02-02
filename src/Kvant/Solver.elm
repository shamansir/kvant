module Kvant.Solver exposing (..)


import Dict
import Dict exposing (Dict)
import Random

import Kvant.Vec2 as Vec2
import Kvant.Matches exposing (Matches)
import Kvant.Matches as Matches
import Kvant.Plane exposing (Plane(..), Coord, Size)
import Kvant.Plane as Plane
import Kvant.Patterns exposing (PatternId, Frequency, UniquePatterns)
import Kvant.Patterns as Patterns
import Kvant.Neighbours as Neighbours
import Kvant.Neighbours as Dir exposing (Direction(..))


type alias Wave = Plane (Matches PatternId)


type alias Solution = Plane (List Patterns.AtomId)


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
    | Focus Coord PatternId


firstStep : Size -> Random.Seed -> Step
firstStep size seed =
    Step 0 seed size Initial


solve : UniquePatterns -> Step -> Step
solve patterns fromStep =
    let
        succeedingStep = advance patterns fromStep
    in case getStatus succeedingStep of
        Solved _ -> succeedingStep
        Terminated -> succeedingStep
        _ -> solve patterns succeedingStep


solveUntil : MaximumSteps -> UniquePatterns -> Step -> Step
solveUntil (MaximumSteps maxSteps) patterns fromStep =
    let
        succeedingStep = advance patterns fromStep
    in case getStatus succeedingStep of
        Solved _ -> succeedingStep
        Terminated -> succeedingStep
        _ ->
            if not (succeedingStep |> exceeds maxSteps)
                then succeedingStep |> solve patterns
                else succeedingStep |> (updateStatus <| ReachedLimit maxSteps)


advance : UniquePatterns -> Step -> Step
advance patterns (Step _ _ outputSize _ as step) =
    case getStatus step of
        Initial ->
            initWave patterns outputSize
                |> InProgress NotFocused
                |> nextStep (getSeed step) step
        InProgress _ wave ->
            case observe (getSeed step) patterns wave of
                ( Collapsed, oSeed ) ->
                    nextStep oSeed step <| Solved wave
                ( Contradiction, oSeed ) ->
                    nextStep oSeed step <| Terminated
                ( Focus position pattern, oSeed ) ->
                    case wave
                            |> propagate patterns position pattern of
                        newWave ->
                            nextStep oSeed step <| InProgress (FocusedAt position) newWave
        _ -> step


observe
    :  Random.Seed
    -> UniquePatterns
    -> Wave
    -> ( Observation, Random.Seed )
observe seed uniquePatterns wave =
    if wave |> hasAContradiction then
        ( Contradiction, seed )
    else if wave |> isCollapsed then
        ( Collapsed, seed )
    else
        let
            ( result, eSeed ) =
                wave |> findLowestEntropy seed uniquePatterns
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
                                    patternChoiceGenerator =
                                        randomPattern uniquePatterns first tail
                                in
                                    Random.step patternChoiceGenerator cSeed
                                        |> Tuple.mapFirst (Focus coord)
                                        |> Just
                            )
                    )
                |> Maybe.withDefault ( Contradiction, cSeed )


propagate
    :  UniquePatterns
    -> Coord
    -> PatternId
    -> Wave
    -> Wave
propagate uniquePatterns focus pattern wave =
    let
        probe : Coord -> Matches PatternId -> Wave -> Wave
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
                                                        |> matchesAtDir uniquePatterns dir
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
            |> probe focus (Matches.single pattern)


initWave : UniquePatterns -> Size -> Wave
initWave uniquePatterns size =
    Plane.filled size <| Matches.fromList <| Dict.keys uniquePatterns


produce
    :  UniquePatterns
    -> Step
    -> Solution
produce patterns (Step _ _ outputSize status) =
    let
        loadValues : Matches PatternId -> List Patterns.AtomId
        loadValues matches =
            matches
                |> Matches.toList
                |> List.map (\patternId ->
                    patterns
                        |> Dict.get patternId
                        |> Maybe.andThen (.subject >> Plane.get (0, 0))
                    )
                |> List.filterMap identity
                -- if pattern wasn't found or contains no value at this point, it is skipped
        fromWave : Wave -> Plane (List Patterns.AtomId)
        fromWave wave = wave |> Plane.map loadValues
    in
        fromWave <| case status of
            Initial -> initWave patterns outputSize
            InProgress _ wave -> wave
            Solved wave -> wave
            Terminated -> Plane.empty outputSize
            ReachedLimit _ -> Plane.empty outputSize


noiseCoefficient : Float
noiseCoefficient = 0.1


entropyOf : Random.Seed -> UniquePatterns -> Matches PatternId -> ( Maybe Float, Random.Seed )
entropyOf seed uniquePatterns matches =
    case Matches.count matches of
        0 -> ( Nothing, seed ) -- contradiction
        1 -> ( Just 0, seed )
        count ->
            let
                patternFrequency : PatternId -> Frequency
                patternFrequency patternId =
                    Dict.get patternId uniquePatterns |>
                        Maybe.map .weight |>
                        Maybe.withDefault 0
                -- TODO: prepare frequency lists in advance, before calculation
                weights =
                    matches
                        |> Matches.toList
                        |> List.map patternFrequency
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
    -> UniquePatterns
    -> Wave
    -> ( Maybe Coord, Random.Seed )
findLowestEntropy seed uniquePatterns =
    let
        withEntropy prevSeed matches f =
            matches
                |> entropyOf prevSeed uniquePatterns
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


randomPattern : UniquePatterns -> PatternId -> List PatternId -> Random.Generator PatternId
randomPattern uniquePatterns first others =
    let
        packWithFrequency : PatternId -> ( Frequency, PatternId )
        packWithFrequency pattern =
            ( uniquePatterns
                |> Dict.get pattern
                |> Maybe.map .weight
                |> Maybe.withDefault 0
            , pattern
            )
    in
        Random.weighted
            (packWithFrequency first)
            (others |> List.map packWithFrequency)


matchesAtDir : UniquePatterns -> Direction -> Matches PatternId -> Matches PatternId
matchesAtDir uniquePatterns dir matchesAtFocus =
    matchesAtFocus
        |> Matches.toList
        |> List.map (getMatchesOf uniquePatterns dir)
        |> List.map (Maybe.withDefault Matches.none)
        |> Matches.union


getMatchesOf : UniquePatterns -> Direction -> PatternId -> Maybe (Matches PatternId)
getMatchesOf uniquePatterns dir pattern =
    uniquePatterns
        |> Dict.get pattern
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


extractMatchesAt : Coord -> Step -> Maybe (Dir.Neighbours (Matches PatternId))
extractMatchesAt coord (Step _ _ _ status) =
    case status of
        Initial -> Nothing
        Terminated -> Nothing
        ReachedLimit _ -> Nothing
        InProgress _ wave ->
            wave |> Plane.getNeighboursOr Matches.none coord |> Just
        Solved wave ->
            wave |> Plane.getNeighboursOr Matches.none coord |> Just

