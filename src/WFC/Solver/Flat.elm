module WFC.Solver.Flat exposing (..)


import Random
import Dict


import WFC.Plane exposing (..)
import WFC.Plane.Flat exposing (SearchMethod, findAllSubs, findOccurrence, findMatches)
import WFC.Vec2 exposing (..)
import WFC.Matches exposing (..)
import WFC.Solver exposing (..)
import WFC.Solver as S exposing (init)


init : Options Vec2 -> Plane Vec2 a -> Solver Vec2 a
init options (Plane size _ as source)  =
    S.init
        options
        (findUniquePatterns
                options.patternSearch
                options.patternSize
                source)
        (walker size)
        source


walker : Vec2 -> Walker Vec2
walker ( w, h ) =
    { first = (0, 0)
    , next = always <| always (0, 0)
    , random =
        Random.map2
            Tuple.pair
            (Random.int 0 (w - 1))
            (Random.int 0 (h - 1))
    , all = above (w, h) |> List.concat |> always
    }


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

apply : Plane Vec2 a -> Step Vec2 -> Plane Vec2 a
apply source step = source


applyTracing
    :  Plane Vec2 (Matches PatternId, List a)
    -> Step Vec2
    -> Plane Vec2 (Matches PatternId, List a)
applyTracing source step = source
