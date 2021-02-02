module Kvant.Patterns exposing (..)

import Dict exposing (Dict)

import Kvant.Vec2 exposing (Vec2)
import Kvant.Vec2 as Vec2
import Kvant.Plane exposing (Plane, Offset)
import Kvant.Plane as Plane exposing (..)
import Kvant.Adjacency exposing (Adjacency)
import Kvant.Matches exposing (Matches)
import Kvant.Matches as Matches


type alias PatternId = Int

-- atom can be pixel, color, tile, character, whatever, but `Key` is the integer ID of it
type alias AtomId = Int

type alias Frequency = Float


type alias Pattern = Plane AtomId


type alias UniquePatterns = Adjacency PatternId Pattern


preprocess
    :  Plane.Symmetry
    -> Plane.Boundary
    -> Plane.Size
    -> Plane AtomId
    -> UniquePatterns
preprocess symmetry boundary ofSize inPlane =
    let
        allSubplanes = subPatterns symmetry boundary ofSize inPlane
        uniquePatterns = evaluateWeights allSubplanes
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
                    { weight = frequency
                    , subject = pattern
                    , matches =
                        findMatches
                            onlyPatternsDict
                            pattern
                    }
                )



evaluateWeights
    :  List Pattern
    -> List (Frequency, Pattern)
evaluateWeights subPlanes_ =
    let
        uniquePatterns =
            subPlanes_
                |> List.foldr
                    (\pattern uniqueOthers ->
                        if pattern |> isAmong uniqueOthers
                            then uniqueOthers
                            else pattern :: uniqueOthers
                    )
                    []
        withOccurrence =
             uniquePatterns
                 |> List.map
                     (
                         \subPlane_ ->
                             ( subPlanes_
                                 |> List.filter (Plane.equal subPlane_)
                                 |> List.length
                             , subPlane_
                             )
                     )
                 |> List.sortBy Tuple.first
        total =
             withOccurrence
                |> List.foldl
                    (\(occurrence, _) sum ->
                         sum + occurrence
                    )
                    0
                 |> toFloat
        _ =
            withOccurrence
                |> List.map (Tuple.mapSecond Plane.toList)
    in
        withOccurrence
            |> List.map
                (\(occurred, v) ->
                    ( toFloat occurred / total
                    , v
                    )
                )


overlappingCoords : Offset -> Plane.Size -> List Vec2
overlappingCoords ( offX, offY ) ( width, height ) =
    Vec2.coordsFlat ( width, height )
        |> List.foldr
            (\(x, y) prev ->
                if (x + offX >= 0) &&
                   (y + offY >= 0) &&
                   (x + offX < width) &&
                   (y + offY < height) then (x + offX, y + offY) :: prev else prev
            )
            []


matchesAt : Vec2 -> Dict PatternId Pattern -> Pattern -> List PatternId
matchesAt offset patterns (Plane size _ as pattern) =
    let
        oCoords = overlappingCoords offset size
    in patterns
        |> Dict.foldr
            (\idx otherPattern matches -> -- ensure plane is the same size as the source
                if otherPattern
                    |> Plane.shift offset
                    |> Plane.equalAt oCoords pattern
                    then idx :: matches
                    else matches
            )
            []


offsetsFor : Plane.Size -> List Offset
offsetsFor ( width, height ) =
    Vec2.rectFlat
        { from = ( -1 * (width - 1),  -1 * (height - 1) )
        , to = ( width - 1, height - 1 )
        }


findMatches : Dict PatternId Pattern -> Pattern -> Dict Offset (Matches PatternId)
findMatches patterns (Plane size _ as pattern) =
    offsetsFor size
        |> List.map (\offset -> ( offset, pattern |> matchesAt offset patterns |> Matches.fromList))
        |> Dict.fromList


isAmong : List Pattern -> Pattern -> Bool
isAmong planes subject =
    planes
        |> List.foldl
                (\other wasBefore ->
                    wasBefore || Plane.equal subject other
                )
           False

subPatterns
    :  Plane.Symmetry
    -> Plane.Boundary
    -> Plane.Size
    -> Pattern
    -> List Pattern
subPatterns symmetry method ofSize pattern =
    pattern
    |> Plane.coords
    |>  (List.map <|
            case method of
                Periodic ->
                    \coord ->
                        Plane.periodicSubPlaneAt coord ofSize pattern
                Bounded ->
                    \coord ->
                        Plane.subPlaneAt coord ofSize pattern
        )
    |> List.filter filledWithValues
    |> List.concatMap (Plane.views symmetry)

