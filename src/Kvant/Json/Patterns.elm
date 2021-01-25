module Kvant.Json.Patterns exposing (..)

import Dict
import Json.Encode as E


import Kvant.Patterns as P
import Kvant.Plane as Plane


encode : P.UniquePatterns -> E.Value
encode =
    Dict.toList
        >> E.list encodePatternStats


encodePattern : P.Pattern -> E.Value
encodePattern =
    Plane.toList2d
        >> E.list
            (E.list (Maybe.withDefault -1 >> E.int))


encodePatternStats : ( P.PatternId, P.PatternWithStats ) -> E.Value
encodePatternStats ( patternId, stats ) =
    E.object
        [ ( "id", E.int patternId )
        , ( "pattern", stats.pattern |> encodePattern )
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

