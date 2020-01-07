module WFC.Solver exposing (..)


-- import Array exposing (Array)
import Dict
import Dict exposing (Dict)
import Random

import WFC.Vec2 exposing (..)
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
type alias Wave v = Plane v (List PatternId)


type AdvanceRule
    = MaximumAttempts Int
    | AdvanceManually


type CellState
    = NoMatches
    | Entropy Float


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
    = Collapsed
    | Contradiction
    | Next v


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
            case observe walker patterns ( seed, wave ) of
                ( oSeed, Collapsed ) ->
                    nextStep step oSeed <| Solved wave
                ( oSeed, Contradiction ) ->
                    nextStep step oSeed <| Terminated
                ( oSeed, Next position ) ->
                    case propagate walker position ( oSeed, wave ) of
                        ( pSeed, newWave ) ->
                            let
                                next = nextStep step pSeed <| InProgress newWave
                            in case options.advanceRule of
                                AdvanceManually -> next
                                MaximumAttempts maxAttempts ->
                                    if not (step |> exceeds maxAttempts)
                                    then next |> solve solver
                                    else next |> (updateStatus <| Exceeded maxAttempts)
    in
        case getStatus step of
            Initial ->
                advance <| initWave source
            InProgress wave ->
                advance wave
            _ -> step


entropyOf : Random.Seed -> UniquePatterns v a -> List PatternId -> (Random.Seed, CellState)
entropyOf seed uniquePatterns patterns =
    case List.length patterns of
        0 -> (seed, NoMatches) -- contradiction
        1 -> (seed, Entropy 0)
        _ ->
            if (List.length patterns == Dict.size uniquePatterns)
                then (seed, Entropy 1)
            else
                let
                    patternFrequency : PatternId -> Maybe Frequency
                    patternFrequency patternId =
                        Dict.get patternId uniquePatterns |>
                            Maybe.map .frequency |>
                            Maybe.andThen Tuple.second
                    -- TODO: prepare frequency lists in advance, before calculation
                    weights =
                        patterns
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
                    , Entropy pureEntropy
                    )

findMinimumEntropy
    :  UniquePatterns v a
    -> Walker v
    -> ( Random.Seed, Wave v )
    -> ( Random.Seed, Maybe v, CellState )
findMinimumEntropy uniquePatterns { all } ( seed, (Plane _ waveF) ) =
    List.foldl
        (\curCoord ( prevSeed, maybePrevCoord, prevState ) ->
            case
                ( prevState
                , waveF curCoord
                )
                of
                ( NoMatches, _ )
                    -> ( prevSeed, maybePrevCoord, prevState )
                ( Entropy _, Nothing )
                    -> ( prevSeed, maybePrevCoord, prevState )
                ( Entropy prevMinEntropy, Just matches )
                    ->
                        case matches |> entropyOf prevSeed uniquePatterns of
                            ( nextSeed, NoMatches ) ->
                                ( nextSeed, Nothing, NoMatches )
                            ( nextSeed, Entropy curEntropy ) ->
                                if curEntropy < prevMinEntropy then
                                    ( nextSeed, Just curCoord, Entropy curEntropy )
                                else ( nextSeed, maybePrevCoord, prevState )
        )
        ( seed, Nothing, Entropy 1 )
        ( all () )


observe
    :  Walker v
    -> UniquePatterns v a
    -> ( Random.Seed, Wave v )
    -> ( Random.Seed, Observation v )
observe walker uniquePatterns ( seed, wave ) =
    let
        ( nextSeed, maybeCoord, result ) =
            ( seed, wave ) |> findMinimumEntropy uniquePatterns walker
                -- FIXME: if Walker will be the part of every Plane
                -- , then we won't need to pass it inside and just use
                -- Walker's folding mechanics
    in ( nextSeed, Collapsed )


propagate : Walker v -> v -> ( Random.Seed, Wave v ) -> ( Random.Seed, Wave v )
propagate _ coord ( seed, wave ) = ( seed, wave )


initWave : Plane v a -> Wave v
initWave (Plane size _) = Plane.empty size


render : Step v -> Plane v a -> Plane v a
render step source = source


renderTracing : Step v -> Plane v a -> Plane v (CellState, List a)
renderTracing step (Plane size _) = Plane.empty size


getSource : Solver v a -> Plane v a
getSource (Solver { source }) = source


getSeed : Step v -> Random.Seed
getSeed (Step _ seed _) = seed


getStatus : Step v -> StepStatus v
getStatus (Step _ _ status) = status


getCount : Step v -> Int
getCount (Step n _ _) = n


nextStep : Step v -> Random.Seed -> StepStatus v -> Step v
nextStep (Step n _ _) seed status = Step (n + 1) seed status


updateStatus : StepStatus v -> Step v -> Step v
updateStatus status (Step n seed _) = Step n seed status


exceeds : Int -> Step v -> Bool
exceeds count (Step stepN _ _) = count <= stepN


waveCellToMaybe : CellState -> Maybe Float
waveCellToMaybe state =
    case state of
        NoMatches -> Nothing
        Entropy entropy -> Just entropy


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
