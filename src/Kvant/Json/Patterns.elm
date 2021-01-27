module Kvant.Json.Patterns exposing (..)

import Dict exposing (Dict)
import Json.Encode as E
import Json.Decode as D


import Kvant.Vec2 as Vec2
import Kvant.Patterns as P
import Kvant.Plane as Plane
import Kvant.Occurrence exposing (Occurrence(..), Frequency(..))


decode : D.Decoder P.UniquePatterns
decode =
    D.list decodePatternStats
        |> D.map Dict.fromList


decodePattern : D.Decoder P.Pattern
decodePattern =
    D.array (D.array D.int)
        |> D.map
            (\grid ->
                Plane.fromArray2d
                    (Vec2.loadSize grid |> Maybe.withDefault (0, 0))
                    grid
            )


decodePatternStats : D.Decoder ( P.PatternId, P.PatternWithStats )
decodePatternStats =
    D.map2
        Tuple.pair
        (D.field "id" D.int)
        <| D.map3
            P.PatternWithStats
            (D.field "pattern" decodePattern)
            (D.map2
                Tuple.pair
                (D.field "occurrence" decodeOccurrence)
                (D.field "frequency"
                    (D.float
                        |> D.map
                            (\v ->
                                if v < 0
                                    then Nothing
                                    else Just <| Frequency v
                            )
                    )
                )
            )
            (D.field "matches" decodeMatches)


decodeOccurrence : D.Decoder Occurrence
decodeOccurrence =
    D.int
        |> D.map (\v -> if v < 0 then Unknown else Times v)


decodeFrequency : D.Decoder (Maybe Frequency)
decodeFrequency =
    D.float
        |> D.map (\v -> if v < 0 then Nothing else Just v)
        |> D.map (Maybe.map Frequency)


decodeMatches : D.Decoder (Dict Plane.Offset (List P.PatternId))
decodeMatches =
    D.list
        (D.map3
            (\x y matches -> ( (x, y), matches ))
            (D.field "x" D.int)
            (D.field "y" D.int)
            (D.field "matches" <| D.list D.int)
        )
        |> D.map Dict.fromList


encode : P.UniquePatterns -> E.Value
encode =
    Dict.toList
        >> E.list encodePatternStats


encodePattern : P.Pattern -> E.Value
encodePattern =
    Plane.toList2d
        >> E.list
            (E.list (Maybe.withDefault -1 >> E.int))


encodeOccurence : Occurrence -> E.Value
encodeOccurence occurence =
    E.int
        <| case occurence of
            Unknown -> -1
            Times n -> n


encodeFrequency : Frequency -> E.Value
encodeFrequency (Frequency v) =
    E.float v


encodePatternStats : ( P.PatternId, P.PatternWithStats ) -> E.Value
encodePatternStats ( patternId, stats ) =
    E.object
        [ ( "id", E.int patternId )
        , ( "pattern", stats.pattern |> encodePattern )
        , ( "occurrence", stats.frequency |> Tuple.first |> encodeOccurence )
        ,
            ( "frequency"
            , stats.frequency
                |> Tuple.second
                |> Maybe.withDefault (Frequency -1)
                |> encodeFrequency
            )
        ,
            ( "matches"
            , stats.matches
                |> Dict.toList
                |> E.list
                    (\((x, y), matches) ->
                        E.object
                            [ ( "x", E.int x )
                            , ( "y", E.int y )
                            , ( "matches", matches |> E.list E.int )
                            ]
                    )
            )
        ]

