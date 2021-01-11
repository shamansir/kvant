module Kvant.Solver exposing (..)


-- import Array exposing (Array)
import Dict
import Dict exposing (Dict)
import Random

import Kvant.Vec2 exposing (..)
import Kvant.Matches exposing (Matches)
import Kvant.Matches as Matches exposing (..)
import Kvant.Occurrence exposing (Occurrence, Frequency, frequencyToFloat)
import Kvant.Occurrence as Occurrence
import Kvant.Plane exposing (Plane(..))
import Kvant.Plane as Plane exposing (map)
{- import Kvant.Plane.Flat as Plane
    exposing ( Boundary, Symmetry, foldl, coords, equal, sub, findMatches, findSubs, findSubsAlt, findOccurrence ) -}
-- import Kvant.Plane as CPlane exposing (fromDict, toDict)
-- import Kvant.Plane.Offset exposing (OffsetPlane(..), toOffset)
-- import Kvant.Plane.Offset as OffsetPlane exposing (get)
import Kvant.Neighbours as Neighbours exposing (..)
import Kvant.Neighbours exposing (Neighbours)
import Kvant.Neighbours as Dir exposing (Direction(..))
import Kvant.Solver.Options exposing (..)


-- value can be pixel, color, tile, character, whatever, but `Key` is the integer ID of it
type alias Key = Int
type alias Pattern = Plane Key
type alias Wave = Plane (Matches PatternId)


-- type alias Rules v a = { }


type Step
    = Step Int Random.Seed StepStatus


type StepStatus
    = Initial Options
    | InProgress FocusState Wave
    | Solved Wave
    | Terminated -- terminated by contradiction
    | ReachedLimit Int


type FocusState
    = NotFocused
    | FocusedAt Vec2


type MaximumSteps = MaximumSteps Int


type alias PatternId = Int


type alias PatternWithStats =
    { pattern : Pattern
    , frequency : ( Occurrence, Maybe Frequency )
    , matches : Plane (List PatternId)
    --, rotations : Dict Rotation PatternId
    }


type alias UniquePatterns =
    Dict PatternId PatternWithStats


-- type alias FindMatches v a =
--     Wave v -> v -> Direction -> Matches a


type Observation
    = Unknown
    | Collapsed
    | Contradiction
    | Focus Vec2 PatternId


preprocess : Plane Key -> UniquePatterns
preprocess _ = Dict.empty -- FIXME


firstStep : Random.Seed -> Step
firstStep seed =
    Step 0 seed Initial


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
advance patterns step =
    case getStatus step of
        Initial { outputSize } ->
            initWave patterns outputSize
                |> InProgress NotFocused
                |> nextStep (getSeed step) step
        InProgress _ wave ->
            case observe (getSeed step) patterns wave of
                ( Collapsed, oSeed ) ->
                    nextStep oSeed step <| Solved wave
                ( Contradiction, oSeed ) ->
                    nextStep oSeed step <| Terminated
                ( Unknown, oSeed ) ->
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
    if wave |> hasAContradiction walker then
        ( Contradiction, seed )
    else if wave |> isWaveCollapsed walker then
        ( Collapsed, seed )
    else
        let
            ( result, eSeed ) =
                wave |> findLowestEntropy seed uniquePatterns walker
            ( coord, cSeed ) =
                case result of
                    Just c -> ( c, eSeed )
                    Nothing ->
                        Random.step walker.random eSeed
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
    :  UniquePatterns v a
    -> Walker v
    -> v
    -> PatternId
    -> Wave v
    -> Wave v
propagate uniquePatterns walker focus pattern wave =
    let
        probe : v -> Matches PatternId -> Wave v -> Wave v
        probe atPos newMatches prevWave =
            let
                curMatches =
                    prevWave
                        |> Plane.get atPos
                        |> Maybe.withDefault Matches.none
                probeNeighbours withWave =
                    Neighbours.cross
                        |> List.foldl
                            (\dir w ->
                                case walker.next atPos dir of
                                    moved ->
                                        -- FIXME: support unbounded planes
                                        if not <| walker.fits moved then w
                                        else
                                            let
                                                curMatchesAtDir =
                                                    w
                                                        |> Plane.get moved
                                                        |> Maybe.withDefault Matches.none
                                                newMatchesAtDir =
                                                    newMatches
                                                        |> matchesAtDir walker uniquePatterns dir
                                                        |> Matches.and curMatchesAtDir
                                            in

                                                w |> probe moved newMatchesAtDir
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


noiseCoefficient : Float
noiseCoefficient = 0.1


entropyOf : Random.Seed -> UniquePatterns v a -> Matches PatternId -> ( Maybe Float, Random.Seed )
entropyOf seed uniquePatterns matches =
    case Matches.count matches of
        0 -> ( Nothing, seed ) -- contradiction
        1 -> ( Just 0, seed )
        count ->
            let
                patternFrequency : PatternId -> Maybe Frequency
                patternFrequency patternId =
                    Dict.get patternId uniquePatterns |>
                        Maybe.map .frequency |>
                        Maybe.andThen Tuple.second
                -- TODO: prepare frequency lists in advance, before calculation
                weights =
                    matches
                        |> Matches.toList
                        |> List.map patternFrequency
                        |> List.filterMap identity
                        |> List.map frequencyToFloat
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
    -> UniquePatterns v a
    -> Walker v
    -> Wave v
    -> ( Maybe v, Random.Seed )
findLowestEntropy seed uniquePatterns { all } (Plane _ waveF) =
    let
        withEntropy prevSeed matches f =
            matches
                |> entropyOf prevSeed uniquePatterns
                |> Tuple.mapFirst (Maybe.andThen f)
        foldingF curCoord ( maybePrevLowest, prevSeed ) =
            case ( waveF curCoord, maybePrevLowest ) of
                ( Nothing, _ ) ->
                    ( maybePrevLowest, prevSeed )
                ( Just matches, Nothing ) ->
                    withEntropy prevSeed matches
                        <| \curEntropy ->
                            if curEntropy > 0
                            then Just ( curCoord, curEntropy )
                            else maybePrevLowest
                ( Just matches, Just ( _, prevMinEntropy ) ) ->
                    withEntropy prevSeed matches
                        <| \curEntropy ->
                            if curEntropy > 0 && curEntropy < prevMinEntropy
                            then Just ( curCoord, curEntropy )
                            else maybePrevLowest
    in
        List.foldl
            foldingF
            ( Nothing, seed )
            ( all () )
            |> Tuple.mapFirst (Maybe.map Tuple.first)
                -- FIXME: if Walker will be the part of every Plane
                -- , then we won't need to pass it inside and just use
                -- Walker's folding mechanics


 -- TODO: produce several IDs?
randomPattern : UniquePatterns v a -> PatternId -> List PatternId -> Random.Generator PatternId
randomPattern uniquePatterns first others =
    let
        packWithFrequency : PatternId -> ( Float, PatternId )
        packWithFrequency pattern =
            ( uniquePatterns
                |> Dict.get pattern
                |> Maybe.andThen (.frequency >> Tuple.second)
                |> Maybe.map frequencyToFloat
                |> Maybe.withDefault 0
            , pattern
            )
    in
        Random.weighted
            (packWithFrequency first)
            (others |> List.map packWithFrequency)


matchesAtDir : Walker v -> UniquePatterns v a -> Direction -> Matches PatternId -> Matches PatternId
matchesAtDir walker uniquePatterns dir matchesAtFocus =
    matchesAtFocus
        |> Matches.toList
        |> List.map (getMatchesOf walker uniquePatterns dir)
        |> List.map (Maybe.withDefault Matches.none)
        |> Matches.union


getMatchesOf : Walker v -> UniquePatterns v a -> Direction -> PatternId -> Maybe (Matches PatternId)
getMatchesOf walker uniquePatterns dir pattern =
    uniquePatterns
        |> Dict.get pattern
        |> Maybe.andThen
            (\{ matches } ->
                let
                    movedPos = walker.next walker.first dir
                in
                    matches
                        |> OffsetPlane.get (movedPos |> toOffset)
            )
        |> Maybe.map Matches.fromList


hasAContradiction : Wave -> Bool
hasAContradiction wave =
    Plane.all
        >> List.foldl
            (\matches wasAContradiction ->
                wasAContradiction || Matches.isNone matches
            )
            False


isWaveCollapsed : Wave -> Bool
isWaveCollapsed =
    Plane.all
        >> List.foldl
            (\matches wasCollapsed ->
                wasCollapsed && (Matches.count matches == 1)
            )
            True


loadFrequencies : UniquePatterns -> Dict PatternId (Maybe Frequency)
loadFrequencies = Dict.map <| always <| (.frequency >> Tuple.second)


initWave : UniquePatterns -> Vec2 -> Wave
initWave uniquePatterns size =
    -- Dict.keys uniquePatterns >> Matches.fromList >> Plane.filled
    Plane.filled size <| Matches.fromList <| Debug.log "patternCount" <| Dict.keys uniquePatterns


apply
    :  (Matches PatternId -> List a -> x)
    -> Solver v a
    -> Step v
    -> Plane v x
apply f (Solver { patterns, walker, source, outputSize }) (Step _ _ status) =
    let
        loadValues : Matches PatternId -> List a
        loadValues matches =
            matches
                |> Matches.toList
                |> List.map (\patternId ->
                    patterns
                        |> Dict.get patternId
                        |> Maybe.andThen (\p -> Plane.get walker.first p.pattern)
                    )
                |> List.filterMap identity
                -- if pattern wasn't found or contains no value at this point, it is skipped
        fromWave : Wave v -> Plane v x
        fromWave wave = wave |> Plane.map (\matches -> f matches <| loadValues matches)
    in
        fromWave <| case status of
            Initial -> initWave patterns outputSize
            InProgress _ wave -> wave
            Solved wave -> wave
            Terminated -> Plane.empty outputSize
            ReachedLimit _ -> Plane.empty outputSize


getSeed : Step -> Random.Seed
getSeed (Step _ seed _) = seed


changeSeedTo : Random.Seed -> Step -> Step
changeSeedTo newSeed (Step n seed step) = Step n newSeed step


getStatus : Step -> StepStatus
getStatus (Step _ _ status) = status


getCount : Step -> Int
getCount (Step n _ _) = n


nextStep : Random.Seed -> Step -> StepStatus -> Step
nextStep seed (Step n _ _) status = Step (n + 1) seed status


updateStatus : StepStatus -> Step -> Step
updateStatus status (Step n seed _) = Step n seed status


exceeds : Int -> Step -> Bool
exceeds count (Step stepN _ _) = count <= stepN


{-
neighboursAt : Direction -> List (PatternId, Pattern v a) -> Pattern v a -> List PatternId
neighboursAt dir from (Pattern size f) =
    []


findNeighbours : List (PatternId, Pattern v a) -> Pattern v a -> Neighbours (List PatternId)
findNeighbours from pattern =
    Neighbours
        (neighboursAt NW from pattern) (neighboursAt N from pattern) (neighboursAt NE from pattern)
        (neighboursAt W  from pattern)                               (neighboursAt  E from pattern)
        (neighboursAt SW from pattern) (neighboursAt S from pattern) (neighboursAt SE from pattern)
-}
