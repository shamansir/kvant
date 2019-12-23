module WFC.Solver exposing (..)


-- import Array exposing (Array)
import Random

import WFC.Plane exposing (..)
import WFC.Plane as Plane exposing (foldl, coords, equal, sub)


type Occured
    = Unknown
    | Times Int


type Cell
    = Contradiction
    | Entropy Int


type Neighbours a =
    Neighbours
        a a a
        a   a
        a a a
type Direction
    = NW | N | NE
    | W  | X |  E
    | SW | S | SE


type Pattern v a = Pattern v (v -> Maybe a)
type Wave v = Wave v (v -> Maybe Cell)


type Approach
    = Overlapping
    | Tiled {- Rules -}


type PatternSearchMethod
    = Bounded
    | Periodic


type alias Options v =
    { approach: Approach
    , patternSearch: PatternSearchMethod
    , patternSize: v
    , inputSize: v
    , outputSize: v
    }


type Step a
    = Step Int Random.Seed


type PatternId = PatternId Int


type alias UniquePatterns v a =
    List
        { occured : Occured
        , id : PatternId
        , pattern : Pattern v a
        , neighbours : Neighbours (List PatternId)
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


isAmong : List (Plane Vec2 a) -> Plane Vec2 a -> Bool
isAmong planes subject =
    planes
        |> List.foldl
                (\other wasBefore ->
                    wasBefore
                        || Plane.equal subject other
                )
           False


memberAt : List (Plane Vec2 a) -> Plane Vec2 a -> Maybe Int
memberAt planes subject =
    planes
        |> List.indexedMap Tuple.pair
        |> List.foldl
                (\(idx, other) wasBefore ->
                    case wasBefore of
                        Just _ -> wasBefore
                        Nothing ->
                            if Plane.equal subject other then
                                Just idx
                            else Nothing
                )
           Nothing


findUniquePatterns : PatternSearchMethod -> Vec2 -> Plane Vec2 a -> UniquePatterns Vec2 a
findUniquePatterns method ofSize inPlane =
    let
        -- FIXME: move each part to a separate function
        subplanes =
            inPlane
                |> allViews
                |> List.concatMap
                    (\view ->
                        Plane.coords view
                            |> case method of
                                Periodic ->
                                    List.map (\coord -> Plane.periodicSubAt coord ofSize view)
                                Bounded ->
                                    List.map (\coord -> Plane.subAt coord ofSize view)
                                    >> List.filterMap identity
                    )
        {-
        subplanes =
            inPlane
                |> Plane.coords
                |> (case method of
                    Periodic ->
                        List.map (\coord -> Plane.periodicSubAt coord ofSize inPlane)
                    Bounded ->
                        List.map (\coord -> Plane.subAt coord ofSize inPlane)
                        >> List.filterMap identity)
                |> List.concatMap allViews
        -}
        uniqueSubplanes =
            subplanes
                |> List.foldl
                    (\pattern uniqueOthers ->
                        if isAmong uniqueOthers pattern
                            then uniqueOthers
                            else pattern :: uniqueOthers
                    )
                    []
        uniqueSubplanesWithFreq =
            uniqueSubplanes
                |> List.map
                    (
                        \subPlane ->
                            ( subplanes
                                |> List.filter (equal subPlane)
                                |> List.length
                                |> times
                            , subPlane
                            )
                   )
        uniquePatternsWithIds =
            uniqueSubplanes
                |> List.map toPattern
                |> List.indexedMap (\index pattern -> Tuple.pair index pattern)
                |> List.map (Tuple.mapFirst PatternId)
    in
        uniqueSubplanesWithFreq
            |> List.map (Tuple.mapSecond toPattern)
            |> List.indexedMap (\index (occurence, pattern) ->
                { occured = occurence
                , pattern = pattern
                , id = PatternId index
                , neighbours = findNeighbours uniquePatternsWithIds pattern
                }
            )
            |> List.sortBy (.occured >> occuredToInt)


noNeighbours : Neighbours (List PatternId)
noNeighbours =
    Neighbours
        [] [] []
        []    []
        [] [] []


neighboursAt : Direction -> List (PatternId, Pattern v a) -> Pattern v a -> List PatternId
neighboursAt dir from (Pattern size f) =
    []


findNeighbours : List (PatternId, Pattern v a) -> Pattern v a -> Neighbours (List PatternId)
findNeighbours from pattern =
    Neighbours
        (neighboursAt NW from pattern) (neighboursAt N from pattern) (neighboursAt NE from pattern)
        (neighboursAt W  from pattern)                               (neighboursAt  E from pattern)
        (neighboursAt SW from pattern) (neighboursAt S from pattern) (neighboursAt SE from pattern)


toPattern : Plane v a -> Pattern v a
toPattern (Plane size f) = Pattern size f


fromPattern : Pattern v a -> Plane v a
fromPattern (Pattern size f) = Plane size f


occuredToInt : Occured -> Int
occuredToInt occured =
    case occured of
        Unknown -> 0
        Times howMuch -> howMuch


compareOccured : Occured -> Occured -> Order
compareOccured a b =
    compare (occuredToInt a) (occuredToInt b)


once : Occured
once = Times 1


times : Int -> Occured
times = Times


inc : Occured -> Occured
inc occured =
    case occured of
        Unknown -> Times 1
        Times before -> Times <| before + 1
