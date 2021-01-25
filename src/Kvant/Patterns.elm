module Kvant.Patterns exposing (..)

import Dict exposing (Dict)

import Kvant.Vec2 exposing (Vec2)
import Kvant.Vec2 as Vec2
import Kvant.Plane exposing (Plane, Offset)
import Kvant.Plane as Plane exposing (..)
import Kvant.Occurrence exposing (..)
import Kvant.Occurrence as Occurrence


type alias PatternId = Int

-- value can be pixel, color, tile, character, whatever, but `Key` is the integer ID of it
type alias Key = Int


type alias Pattern = Plane Key


type alias PatternWithStats =
    { pattern : Pattern
    , frequency : ( Occurrence, Maybe Frequency )
    , matches : Dict Offset (List PatternId)
    --, rotations : Dict Rotation PatternId
    }


type alias UniquePatterns =
    Dict PatternId PatternWithStats


preprocess
    :  Plane.Symmetry
    -> Plane.Boundary
    -> Plane.Size
    -> Plane Key
    -> UniquePatterns
preprocess symmetry boundary ofSize inPlane =
    let
        allSubplanes = subPatterns symmetry boundary ofSize inPlane
        uniquePatterns = evaluateOccurrence allSubplanes
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



evaluateOccurrence
    :  List Pattern
    -> List ((Occurrence, Maybe Frequency), Pattern)
evaluateOccurrence subPlanes_ =
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
                                 |> Occurrence.times
                             , subPlane_
                             )
                     )
                 |> List.sortBy (Tuple.first >> Occurrence.toInt)
        total =
             withOccurrence
                |> List.foldl
                    (\(occurrence, _) sum ->
                         sum + Occurrence.toInt occurrence
                    )
                    0
                 |> toFloat
        _ =
            withOccurrence
                |> List.map (Tuple.mapSecond Plane.toList)
    in
        withOccurrence
            |> List.map
                (\(occurrence, v) ->
                    (
                        ( occurrence
                        , occurrence
                            |> Occurrence.toMaybe
                            |> Maybe.map
                                (\occurred ->
                                    Occurrence.frequencyFromFloat <| toFloat occurred / total))
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


findMatches : Dict PatternId Pattern -> Pattern -> Dict Offset (List PatternId)
findMatches patterns (Plane size _ as pattern) =
    offsetsFor size
        |> List.map (\offset -> ( offset, pattern |> matchesAt offset patterns ))
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

