module WFC.Solver exposing (..)


-- import Array exposing (Array)
import Dict
import Dict exposing (Dict)
import Random

import WFC.Vec2 exposing (..)
import WFC.Occurrence exposing (Occurrence, Frequency)
import WFC.Occurrence as Occurrence
import WFC.Plane.Plane exposing (Plane(..), N(..))
import WFC.Plane.Plane as Plane exposing (map)
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
type alias Wave v = Plane v (State, List PatternId)


type AdvanceRule
    = MaximumAttempts Int
    | AdvanceManually


type State
    = Contadiction
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


type alias Walker v =
    { next : Direction -> v
    , random : Random.Generator v
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


initFlat : Plane Vec2 a -> Options Vec2 -> Solver Vec2 a
initFlat (Plane size _ as source) options =
    init
        options
        (findUniquePatterns
                options.patternSearch
                options.patternSize
                source)
        (flatWalker size)
        source


flatWalker : Vec2 -> Walker Vec2
flatWalker ( w, h ) =
    { next = always (0, 0)
    , random =
        Random.map2
            Tuple.pair
            (Random.int 0 (w - 1))
            (Random.int 0 (h - 1))
    }


solve : Solver v a -> Step v -> Step v
solve (Solver { options, source, patterns, walker } as solver) step  =
    let
        seed = getSeed step
        advance wave =
            case observe walker ( seed, wave ) of
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


initWave : Plane v a -> Wave v
initWave (Plane size _) = Plane.empty size


apply : Plane v a -> Step v -> Plane v a
apply source step = source


minimumEntropy : Wave Vec2 -> Maybe Vec2
minimumEntropy wave =
    Plane.foldl
        (\cell prev -> Just (0, 0))
        Nothing
        wave


observe : Walker v -> ( Random.Seed, Wave v ) -> ( Random.Seed, Observation v )
observe { next } ( seed, wave ) = ( seed, Next <| next S )


propagate : Walker v -> v -> ( Random.Seed, Wave v ) -> ( Random.Seed, Wave v )
propagate _ coord ( seed, wave ) = ( seed, wave )


findUniquePatterns
    :  SearchMethod
    -> N Vec2
    -> Plane Vec2 a
    -> UniquePatterns Vec2 a
findUniquePatterns method ofSize inPlane =
    let
        allSubplanes = findAllSubs method ofSize inPlane
        uniquePatterns = findOccurrence allSubplanes
        uniquePatternsDict =
            uniquePatterns
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
        onlyPatternsDict =
            uniquePatternsDict
                |> Dict.map (always Tuple.second)
    in
        uniquePatternsDict
                |> Dict.map (\_ ( frequency, pattern ) ->
                        { frequency = frequency
                        , pattern = pattern
                        , matches =
                            findMatches
                                onlyPatternsDict
                                pattern
                        }
                    )


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


type alias TextOptions = Options Vec2
type alias TextSolver = Solver Vec2 Char
