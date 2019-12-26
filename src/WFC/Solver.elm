module WFC.Solver exposing (..)


-- import Array exposing (Array)
import Dict
import Dict exposing (Dict)
import Random

import WFC.Occured exposing (Occured)
import WFC.Occured as Occured
import WFC.Plane exposing (..)
import WFC.Plane as Plane exposing (foldl, coords, equal, sub, findMatches, map)
import WFC.Neighbours exposing (..)


type Cell
    = Contradiction
    | Entropy Int


type Pattern v a = Pattern v (v -> Maybe a)
type Wave v = Wave v (v -> Maybe Cell)


type Approach
    = Overlapping
    | Tiled {- Rules -}


type alias Options v =
    { approach: Approach
    , patternSearch: SearchMethod
    , patternSize: N v
    , inputSize: v
    , outputSize: v
    -- add symmetry etc.
    }


type Step a
    = Step Int Random.Seed


type alias PatternId = Int


type alias UniquePatterns v a =
    Dict PatternId
        { pattern: Pattern v a
        , occured : Occured
        , matches : Plane (Offset v) (List PatternId)
        }


type Solver v a =
    Solver
        (Options v)
        (Plane v a) -- source plane
        (UniquePatterns v a) -- pattern statistics


solve : Step a -> Solver v a -> ( Step a, Plane v a )
solve step (Solver options sourcePlane patterns) =
    ( step, sourcePlane )


type alias TextOptions = Options Vec2
type alias TextSolver = Solver Vec2 Char


findUniquePatterns : SearchMethod -> N Vec2 -> Plane Vec2 a -> UniquePatterns Vec2 a
findUniquePatterns method ofSize inPlane =
    let
        allSubplanes = findAllSubs method ofSize inPlane
        uniqueSubplanes = findOccurence allSubplanes
        uniquePatterns =
            uniqueSubplanes
                |> List.map (Tuple.mapSecond toPattern)
        -- uniquePatternsWithIds =
        --     uniquePatterns
        --         |> List.map Tuple.second
        --         |> List.indexedMap Tuple.pair
                -- |> List.map (Tuple.mapFirst PatternId)
        uniquePatternsDict =
            uniquePatterns
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
        onlyPatternsDict =
            uniquePatternsDict
                |> Dict.map (always Tuple.second)

    in
        uniquePatternsDict
                |> Dict.map (\_ ( occurence, pattern ) ->
                        { occured = occurence
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


findMatches
    :  Dict PatternId (Pattern Vec2 a)
    -> Pattern Vec2 a
    -> Plane (Offset Vec2) (List PatternId)
findMatches from for =
    Plane.findMatches
        (from
            |> Dict.map (always fromPattern))
        (fromPattern for)


toPattern : Plane v a -> Pattern v a
toPattern (Plane size f) = Pattern size f


fromPattern : Pattern v a -> Plane v a
fromPattern (Pattern size f) = Plane size f
