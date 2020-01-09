module WFC.Solver exposing (..)


-- import Array exposing (Array)
import Dict
import Dict exposing (Dict)
import Random

import WFC.Vec2 exposing (..)
import WFC.Matches exposing (Matches)
import WFC.Matches as Matches exposing (..)
import WFC.Occurrence exposing (Occurrence, Frequency, frequencyToFloat)
import WFC.Occurrence as Occurrence
import WFC.Plane exposing (Plane(..), N(..))
import WFC.Plane as Plane exposing (map)
import WFC.Plane.Flat as Plane
    exposing ( SearchMethod, foldl, coords, equal, sub, findMatches, findAllSubs, findAllSubsAlt, findOccurrence )
import WFC.Plane.Offset exposing (OffsetPlane(..))
import WFC.Neighbours exposing (..)


type alias Options v =
    { approach : Approach
    , patternSearch : SearchMethod
    , patternSize : N v
    , inputSize : v
    , outputSize : v
    , advanceRule : AdvanceRule
    -- add symmetry etc.
    }


type alias Pattern v a = Plane v a
type alias Wave v = Plane v (Matches PatternId)


type AdvanceRule
    = MaximumAttempts Int
    | AdvanceManually


type Approach
    = Overlapping
    | Tiled {- Rules -}


type Step v
    = Step Int Random.Seed (StepStatus v)


type alias PatternId = Int


type alias UniquePattern v a =
    { pattern : Pattern v a
    , frequency : ( Occurrence, Maybe Frequency )
    , matches : OffsetPlane v (List PatternId)
    }


type alias UniquePatterns v a =
    Dict PatternId (UniquePattern v a)


type UniquePatternsCount = UniquePatternsCount Int


type alias Walker v =
    { next : Direction -> v
    , random : Random.Generator v
    , all : () -> List v
    }


type StepStatus v
    = Initial
    | InProgress (Wave v)
    | Solved (Wave v)
    | Terminated -- terminated by contradiction
    | Exceeded Int


type Observation v
    = Unknown
    | Collapsed
    | Contradiction
    | Focus v PatternId


type Solver v a =
    Solver
        { options : Options v
        , source : Plane v a
        , patterns : UniquePatterns v a -- a function which returns the adjastent matching tiles?
        , walker : Walker v
        }


firstStep : Random.Seed -> Step v
firstStep seed =
    Step 0 seed Initial


init
    :  Options v -- FIXME: only `advanceRule` used in solving atm
    -> UniquePatterns v a
    -> Walker v
    -> Plane v a
    -> Solver v a
init options patterns walker source =
    Solver
        { options = options
        , source = source
        , patterns = patterns
        , walker = walker
        }


solve : Solver v a -> Step v -> Step v
solve (Solver { options, source, patterns, walker } as solver) step  =
    let
        seed = getSeed step
        advance wave =
            case observe seed walker patterns wave of
                ( oSeed, Collapsed ) ->
                    nextStep oSeed step <| Solved wave
                ( oSeed, Contradiction ) ->
                    nextStep oSeed step <| Terminated
                ( oSeed, Unknown ) ->
                    nextStep oSeed step <| Terminated
                ( oSeed, Focus position pattern ) ->
                    case propagate oSeed walker position pattern wave of
                        ( pSeed, newWave ) ->
                            let
                                next = nextStep pSeed step <| InProgress newWave
                            in case options.advanceRule of
                                AdvanceManually -> next
                                MaximumAttempts maxAttempts ->
                                    if not (step |> exceeds maxAttempts)
                                    then next |> solve solver
                                    else next |> (updateStatus <| Exceeded maxAttempts)
    in
        case getStatus step of
            Initial ->
                advance <| initWave patterns source
            InProgress wave ->
                advance wave
            _ -> step


entropyOf : Random.Seed -> UniquePatterns v a -> Matches PatternId -> ( Random.Seed, Maybe Float )
entropyOf seed uniquePatterns matches =
    case Matches.count matches of
        0 -> (seed, Nothing) -- contradiction
        1 -> (seed, Just 0)
        count ->
            if (count == Dict.size uniquePatterns)
                then (seed, Just 1)
            else
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
                    sumOfWeights = List.foldl (+) 0 weights
                    sumOfLoggedWeights =
                        weights
                            |> List.map (logBase 2)
                            |> List.foldl (+) 0
                    pureEntropy =
                        (logBase 2 sumOfWeights) - (sumOfLoggedWeights / sumOfWeights)
                in
                    ( seed
                    , Just pureEntropy -- FIXME: add randomness
                    )


findLowestEntropy
    :  Random.Seed
    -> UniquePatterns v a
    -> Walker v
    -> Wave v
    -> ( Random.Seed, Maybe v )
findLowestEntropy seed uniquePatterns { all } (Plane _ waveF) =
    let
        withEntropy prevSeed matches f =
            matches
                |> entropyOf prevSeed uniquePatterns
                |> Tuple.mapSecond (Maybe.andThen f)
        foldingF curCoord ( prevSeed, maybePrevLowest ) =
            case ( maybePrevLowest, waveF curCoord ) of
                ( _, Nothing ) ->
                    ( prevSeed, maybePrevLowest )
                ( Nothing, Just matches ) ->
                    withEntropy prevSeed matches
                        <| \curEntropy ->
                            if curEntropy > 0
                            then Just ( curCoord, curEntropy )
                            else maybePrevLowest
                ( Just ( _, prevMinEntropy ), Just matches ) ->
                    withEntropy prevSeed matches
                        <| \curEntropy ->
                            if curEntropy > 0 && curEntropy < prevMinEntropy
                            then Just ( curCoord, curEntropy )
                            else maybePrevLowest
    in
        List.foldl
            foldingF
            ( seed, Nothing )
            ( all () )
            |> Tuple.mapSecond (Maybe.map Tuple.first)
                -- FIXME: if Walker will be the part of every Plane
                -- , then we won't need to pass it inside and just use
                -- Walker's folding mechanics


observe
    :  Random.Seed
    -> Walker v
    -> UniquePatterns v a
    -> Wave v
    -> ( Random.Seed, Observation v )
observe seed walker uniquePatterns wave =
    if wave |> hasAContradiction walker then
        ( seed, Contradiction )
    else if wave |> isWaveCollapsed walker then
        ( seed, Collapsed )
    else
        let
            ( nextSeed, result ) =
                wave |> findLowestEntropy seed uniquePatterns walker
        in
            case result of
                Just coord -> ( nextSeed, Focus coord 1 ) -- FIXME: find actual Pattern ID
                Nothing -> ( nextSeed, Focus (walker.next S) 1 ) -- FIXME: find random coordinate


propagate : Random.Seed -> Walker v -> v -> PatternId -> Wave v -> ( Random.Seed, Wave v )
propagate seed _ coord pattern wave = ( seed, wave )


 -- FIXME: Maybe Bool
hasAContradiction : Walker v -> Wave v -> Bool
hasAContradiction { all } (Plane _ waveF) =
    List.foldl
        (\coord wasAContradiction ->
            wasAContradiction ||
                (waveF coord
                    |> Maybe.map Matches.isNone
                    |> Maybe.withDefault True)
        )
        False
        ( all () )


 -- FIXME: Maybe Bool
isWaveCollapsed : Walker v -> Wave v -> Bool
isWaveCollapsed { all } (Plane _ waveF) =
    List.foldl
        (\coord wasCollapsed ->
            wasCollapsed &&
                (waveF coord
                    |> Maybe.map (\matches -> Matches.count matches == 1)
                    |> Maybe.withDefault False)
        )
        True
        ( all () )


initWave : UniquePatterns v a -> Plane v a -> Wave v
initWave uniquePatterns (Plane size _) =
    Plane.filled size <| Matches.fromList <| Dict.keys uniquePatterns


render : Step v -> Plane v a -> Plane v a
render step source = source


renderTracing : Step v -> UniquePatterns v a -> Plane v a -> Plane v (Matches PatternId, List a)
renderTracing (Step _ _ status) uniquePatterns (Plane size _ as source) =
    let
        fromWave : Wave v -> Plane v (Matches PatternId, List a)
        fromWave wave = wave |> Plane.map (\matches -> (matches, []))
    in
        fromWave <| case status of
            Initial -> source |> initWave uniquePatterns
            InProgress wave -> wave
            Solved wave -> wave
            Terminated -> Plane.empty size
            Exceeded _ -> Plane.empty size


getSource : Solver v a -> Plane v a
getSource (Solver { source }) = source


getPatterns : Solver v a -> UniquePatterns v a
getPatterns (Solver { patterns }) = patterns


getSeed : Step v -> Random.Seed
getSeed (Step _ seed _) = seed


getStatus : Step v -> StepStatus v
getStatus (Step _ _ status) = status


getCount : Step v -> Int
getCount (Step n _ _) = n


nextStep : Random.Seed -> Step v -> StepStatus v -> Step v
nextStep seed (Step n _ _) status = Step (n + 1) seed status


updateStatus : StepStatus v -> Step v -> Step v
updateStatus status (Step n seed _) = Step n seed status


exceeds : Int -> Step v -> Bool
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
