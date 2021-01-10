module Kvant.Solver.Flat exposing (..)


import Random
import Dict


import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (Boundary, Symmetry, findSubsAlt, findOccurrence, findMatches)
import Kvant.Vec2 exposing (..)
import Kvant.Matches exposing (..)
import Kvant.Solver exposing (..)
import Kvant.Solver as S exposing (init)
import Kvant.Solver.Options exposing (..)
import Kvant.Neighbours as Dir exposing (Direction(..), move)


init : Options Vec2 -> Plane Vec2 a -> Solver Vec2 a
init options (Plane size _ as source)  =
    S.init
        (case options.approach of
            Overlapping { inputBoundary, patternSize, symmetry } ->
                (findUniquePatterns
                        symmetry
                        inputBoundary
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
        (x >= 0) && (y >= 0) && (x < w) && (y < h)
    }


findUniquePatterns
    :  Symmetry
    -> Boundary
    -> N Vec2
    -> Plane Vec2 a
    -> UniquePatterns Vec2 a
findUniquePatterns symmetry boundary ofSize inPlane =
    let
        allSubplanes = findSubsAlt symmetry boundary ofSize inPlane
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
