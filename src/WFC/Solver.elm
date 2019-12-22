module WFC.Solver exposing (..)


import WFC.Plane exposing (..)
import WFC.Plane as Plane exposing (foldl, coords, equal, sub)


type Occured
    = Unknown
    | Times Int


type Pattern v a = Pattern v (v -> Maybe a)


type Approach
    = Overlapping
    | Tiled {- Rules -}


type alias Options v =
    { approach: Approach
    , patternSize: v
    , inputSize: v
    , outputSize: v
    }


type Step a
    = Step Int


type Solver v a = Solver (Options v) (Plane v a) (List (Occured, Pattern v a))


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


findPatterns : Vec2 -> Plane Vec2 a -> List (Occured, Pattern Vec2 a)
findPatterns ofSize inPlane =
    let
        allSubplanesVariants =
            inPlane
                |> Plane.coords
                |> List.foldl
                    (\coord foundSubplanes ->
                        foundSubplanes ++
                            (Plane.subAt coord ofSize inPlane
                                |> Maybe.map allViews
                                |> Maybe.withDefault [])
                        -- FIXME: consider periodically tiling source pattern
                    )
                    []
        uniqueSubplanes =
            allSubplanesVariants
                |> List.foldl
                    (\pattern uniqueOthers ->
                        if isAmong uniqueOthers pattern
                            then uniqueOthers
                            else pattern :: uniqueOthers
                    )
                    []
        uniqueSubplanesWithFreq =
            uniqueSubplanes
                |> List.map(
                        \subPlane ->
                            ( allSubplanesVariants
                                |> List.filter (equal subPlane)
                                |> List.length
                                |> times
                            , subPlane)
                   )
    in
        uniqueSubplanesWithFreq
            |> List.sortBy (Tuple.first >> occuredToInt)
            |> List.map (Tuple.mapSecond toPattern)


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
