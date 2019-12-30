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
    exposing (SearchMethod, foldl, coords, equal, sub, findMatches, findAllSubs, findAllSubsAlt, findOccurrence)
import WFC.Plane.Offset exposing (OffsetPlane(..))
import WFC.Neighbours exposing (..)


type Cell
    = Contadiction
    | Entropy Float


type alias Pattern v a = Plane v a
type alias Wave v = Plane v (List PatternId)


type Approach
    = Overlapping
    | Tiled {- Rules -}


type alias Options v =
    { approach : Approach
    , patternSearch : SearchMethod
    , patternSize : N v
    , inputSize : v
    , outputSize : v
    -- add symmetry etc.
    }


type Step a
    = Step Int Random.Seed


type alias PatternId = Int


-- type Entropy = Entropy Float


type alias UniquePattern v a =
    { pattern : Pattern v a
    , frequency : ( Occurrence, Maybe Frequency )
    , matches : OffsetPlane v (List PatternId)
    }


type alias UniquePatterns v a =
    Dict PatternId (UniquePattern v a)


type Solver v a =
    Solver
        { options : Options v
        , source : Plane v a
        , patterns : UniquePatterns v a
        , wave : Wave v
        }


initFlat : Plane Vec2 a -> Options Vec2 -> Solver Vec2 a
initFlat (Plane size _ as source) options =
    Solver
        { options = options
        , source = source
        , patterns =
            findUniquePatterns
                options.patternSearch
                options.patternSize
                source
        , wave = Plane.empty size
        }



solve : Step a -> Solver v a -> ( Step a, Plane v a )
solve step (Solver { options, source, patterns }) =
    ( step, source )


type alias TextOptions = Options Vec2
type alias TextSolver = Solver Vec2 Char


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
