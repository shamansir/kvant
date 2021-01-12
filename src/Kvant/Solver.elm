module Kvant.Solver exposing (..)


-- import Array exposing (Array)
import Dict
import Dict exposing (Dict)
import Random

import Kvant.Vec2 exposing (..)
import Kvant.Vec2 as Vec2 exposing (random)
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
    = Step Int Random.Seed Options StepStatus -- Replace Options only by OutputSize and nothing else


type StepStatus
    = Initial
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


firstStep : Options -> Random.Seed -> Step
firstStep options seed =
    Step 0 seed options Initial


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
advance patterns (Step _ _ { outputSize } _ as step) =
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
    -> Vec2
    -> PatternId
    -> Wave
    -> Wave
propagate uniquePatterns focus pattern wave =
    let
        probe : Vec2 -> Matches PatternId -> Wave -> Wave
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
                                case dir |> Dir.move atPos of
                                    moved ->
                                        -- FIXME: support unbounded planes
                                        if not <| Plane.fits moved w then w
                                        else
                                            let
                                                curMatchesAtDir =
                                                    w
                                                        |> Plane.get moved
                                                        |> Maybe.withDefault Matches.none
                                                newMatchesAtDir =
                                                    newMatches
                                                        |> matchesAtDir uniquePatterns dir
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


entropyOf : Random.Seed -> UniquePatterns -> Matches PatternId -> ( Maybe Float, Random.Seed )
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
    -> UniquePatterns
    -> Wave
    -> ( Maybe Vec2, Random.Seed )
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

        Plane.allWithCoords
            >> List.foldl
                foldingF
                ( Nothing, seed )
            >> Tuple.mapFirst (Maybe.map Tuple.first)


 -- TODO: produce several IDs?
randomPattern : UniquePatterns -> PatternId -> List PatternId -> Random.Generator PatternId
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
                matches |> Plane.get (Dir.offsetFor dir)
            )
        |> Maybe.map Matches.fromList


hasAContradiction : Wave -> Bool
hasAContradiction =
    Plane.all
        >> List.foldl
            (\matches wasAContradiction ->
                wasAContradiction || Matches.isNone matches
            )
            False


isCollapsed : Wave -> Bool
isCollapsed =
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
    :  (Matches PatternId -> List Key -> x)
    -> UniquePatterns
    -> Step
    -> Plane x
apply f patterns (Step _ _ { outputSize } status) =
    let
        loadValues : Matches PatternId -> List Key
        loadValues matches =
            matches
                |> Matches.toList
                |> List.map (\patternId ->
                    patterns
                        |> Dict.get patternId
                        |> Maybe.andThen (.pattern >> Plane.get (0, 0))
                    )
                |> List.filterMap identity
                -- if pattern wasn't found or contains no value at this point, it is skipped
        fromWave : Wave -> Plane x
        fromWave wave = wave |> Plane.map (\matches -> f matches <| loadValues matches)
    in
        fromWave <| case status of
            Initial -> initWave patterns outputSize
            InProgress _ wave -> wave
            Solved wave -> wave
            Terminated -> Plane.empty outputSize
            ReachedLimit _ -> Plane.empty outputSize


getSeed : Step -> Random.Seed
getSeed (Step _ seed _ _) = seed


changeSeedTo : Random.Seed -> Step -> Step
changeSeedTo newSeed (Step n seed opts step) = Step n newSeed opts step


getStatus : Step -> StepStatus
getStatus (Step _ _ _ status) = status


getCount : Step -> Int
getCount (Step n _ _ _) = n


nextStep : Random.Seed -> Step -> StepStatus -> Step
nextStep seed (Step n _ options _) status = Step (n + 1) seed options status


updateStatus : StepStatus -> Step -> Step
updateStatus status (Step n seed opts _) = Step n seed opts status


exceeds : Int -> Step -> Bool
exceeds count (Step stepN _ _ _) = count <= stepN


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
