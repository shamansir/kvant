module Kvant.Json.Adjacency exposing (..)


import Dict exposing (Dict)

import Json.Encode as E
import Json.Decode as D

import Kvant.Plane as Plane
import Kvant.Adjacency exposing (Adjacency)
import Kvant.Matches as Matches exposing (Matches)


encode : (a -> E.Value) -> Adjacency Int a -> E.Value
encode itemEncode =
    Dict.toList
        >> E.list (encodeItem itemEncode)


decode : D.Decoder a -> D.Decoder (Adjacency Int a)
decode decodeItem =
    D.list (decodeItems decodeItem)
        |> D.map Dict.fromList


decodeItems
    :  D.Decoder a
    -> D.Decoder
        ( Int
        ,
            { subject : a
            , weight : Float
            , matches : Dict Plane.Offset (Matches Int)
            }
        )
decodeItems decodeItem =
    D.map2
        Tuple.pair
        (D.field "id" D.int)
        <| D.map3
            (\s w m -> { subject = s, weight = w, matches = m })
            (D.field "subject" decodeItem)
            (D.field "weight" D.float)
            (D.field "matches" decodeMatches)


decodeMatches : D.Decoder (Dict Plane.Offset (Matches Int))
decodeMatches =
    D.list
        (D.map3
            (\x y matches -> ( (x, y), Matches.fromList matches ))
            (D.field "x" D.int)
            (D.field "y" D.int)
            (D.field "matches" <| D.list D.int)
        )
        |> D.map Dict.fromList


encodeItem
    :   ( a -> E.Value )
    ->  ( Int
        ,
            { subject : a
            , weight : Float
            , matches : Dict Plane.Offset (Matches Int)
            }
        )
    -> E.Value
encodeItem itemEncode ( itemId, stats ) =
    E.object
        [ ( "id", E.int itemId )
        , ( "subject", stats.subject |> itemEncode )
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

