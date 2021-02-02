module Kvant.Json.Patterns exposing (..)

import Dict exposing (Dict)
import Json.Encode as E
import Json.Decode as D


import Kvant.Vec2 as Vec2
import Kvant.Patterns as P
import Kvant.Plane as Plane
import Kvant.Matches as Matches exposing (Matches)


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


decodePatternStats
    : D.Decoder
        ( P.PatternId
        ,
            { subject : P.Pattern
            , weight : Float
            , matches : Dict Plane.Offset (Matches P.PatternId)
            }
        )
decodePatternStats =
    D.map2
        Tuple.pair
        (D.field "id" D.int)
        <| D.map3
            (\s w m -> { subject = s, weight = w, matches = m })
            (D.field "pattern" decodePattern)
            (D.field "weight" D.float)
            (D.field "matches" decodeMatches)


decodeMatches : D.Decoder (Dict Plane.Offset (Matches P.PatternId))
decodeMatches =
    D.list
        (D.map3
            (\x y matches -> ( (x, y), Matches.fromList matches ))
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


encodePatternStats
    :
        ( P.PatternId
        ,
            { subject : P.Pattern
            , weight : Float
            , matches : Dict Plane.Offset (Matches P.PatternId)
            }
        )
    -> E.Value
encodePatternStats ( patternId, stats ) =
    E.object
        [ ( "id", E.int patternId )
        , ( "pattern", stats.subject |> encodePattern )
        , ( "weight", stats.weight |> E.float )
        ,
            ( "matches"
            , stats.matches
                |> Dict.toList
                |> E.list
                    (\((x, y), matches) ->
                        E.object
                            [ ( "x", E.int x )
                            , ( "y", E.int y )
                            , ( "matches", matches |> Matches.toList |> E.list E.int )
                            ]
                    )
            )
        ]

