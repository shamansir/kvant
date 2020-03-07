module WFC.Solver.Flat exposing (..)


import Random
import Dict


import WFC.Plane exposing (..)
import WFC.Plane.Flat exposing (Boundary, Symmetry, findAllSubs, findOccurrence, findMatches)
import WFC.Vec2 exposing (..)
import WFC.Matches exposing (..)
import WFC.Solver exposing (..)
import WFC.Solver as S exposing (init)
import WFC.Neighbours as Dir exposing (Direction(..), move)


init : Options Vec2 a -> Plane Vec2 a -> Solver Vec2 a
init options (Plane size _ as source)  =
    S.init
        options.advanceRule
        (case options.approach of
            Overlapping { searchBoundary, patternSize } ->
                -- FIXME: use symmetry as well
                (findUniquePatterns
                        searchBoundary
                        patternSize
                        source)
            Tiled ->
                Dict.empty -- FIXME: implement
        )
        options.outputSize
        options.outputBoundary
        (walker options.outputSize)
        source


walker : Vec2 -> Walker Vec2
walker ( w, h ) =
    { first = (0, 0)
    , next = Dir.move
    , random =
        Random.map2
            Tuple.pair
            (Random.int 0 (w - 1))
            (Random.int 0 (h - 1))
    , all = above (w, h) |> List.concat |> always
    , fits = \(x, y) ->
        (x >= 0) && (y >= 0) && (x < w) && (x < h)
    }


findUniquePatterns
    :  Boundary
    -> N Vec2
    -> Plane Vec2 a
    -> UniquePatterns Vec2 a
findUniquePatterns boundary ofSize inPlane =
    let
        allSubplanes = findAllSubs boundary ofSize inPlane
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
