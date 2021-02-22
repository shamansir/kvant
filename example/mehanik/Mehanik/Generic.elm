module Mehanik.Generic exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Kvant.Vec2 as Vec2 exposing (Vec2)
import Kvant.Plane exposing (Plane)
import Kvant.Neighbours as Neighbours exposing (..)
import Kvant.Matches as Matches exposing (Matches)
import Kvant.Direction as Dir


viewClickableArea : Vec2 -> Vec2 -> (Vec2 -> msg) -> Html msg
viewClickableArea ( width, height ) ( itemWidth, itemHeight ) toMsg =
    List.range 0 (height - 1)
        |> List.map
            (\row ->
                    List.range 0 (width - 1)
                        |> List.map (Tuple.pair row)
            )
        |> List.map
            (\row ->
                div
                    [ style "display" "flex", style "flex-direction" "row"
                    -- , style "width" <| String.fromInt itemWidth
                    , style "height" <| String.fromInt itemHeight
                    ]
                    <|
                        List.map (\(y, x) ->
                            div
                                [ style "width" <| String.fromInt itemWidth ++ "px"
                                , style "height" <| String.fromInt itemHeight ++ "px"
                                , style "cursor" "pointer"
                                , onClick <| toMsg (x, y)
                                ]
                                []
                        ) row
            )
        |> div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "position" "absolute"
            , style "z-index" "1111"
            ]


viewMatches : (a -> Html msg) -> Neighbours (Matches a) -> Html msg
viewMatches viewMatch neighbours =
    [ [ Dir.NW, Dir.N, Dir.NE ]
    , [ Dir.W,  Dir.X, Dir.E  ]
    , [ Dir.SW, Dir.S, Dir.SE ]
    ]
    |> List.map
        (\directionsRow ->
            div
                [ style "display" "flex", style "flex-direction" "row"
                , style "margin" "20px 20px"
                ]
                <|
                    List.map (\dir ->

                        div []
                            [ text <| Dir.toString dir
                            , text <| Vec2.toString <| Dir.toOffset dir
                            , div
                                    [ style "display" "flex"
                                    , style "flex-direction" "column"
                                    , style "margin" "20px 20px"
                                    ]
                                    <|
                                        (\list -> case list of
                                            [] -> [ text "NONE" ]
                                            _ -> list
                                        )
                                    <| List.map
                                        (\matchId ->
                                            div
                                                [ style "transform" "scale(0.8)"
                                                , style "margin" "0px 10px"
                                                , style "padding" "3px"
                                                , style "border" "1px solid lightgray"
                                                , style "border-radius" "3px"
                                                ]
                                                [ viewMatch matchId
                                                ]
                                        )
                                    <| Matches.toList
                                    <| Neighbours.get dir neighbours
                            ]

                    ) directionsRow
        )
    |> div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "margin" "10px 0"
        ]


exampleFrame : msg -> List (Html msg) -> Html msg
exampleFrame msg =
    div
        [ style "margin" "5px"
        , style "padding" "10px"
        , style "border" "1px solid black"
        , style "border-radius" "5px"
        , style "background-color" "#f5f5f5"
        , style "cursor" "pointer"
        , onClick msg
        ]


viewList : (a -> msg) -> (a -> Html msg) -> List a -> Html msg
viewList clickHandler viewItem =
    div
        [ style "display" "flex"
        , style "overflow" "scroll"
        ]
        << List.map
            (\item ->
                div
                    [ style "transform" "scale(0.5)"
                    , style "margin" "5px"
                    , style "cursor" "pointer"
                    , style "padding" "3px"
                    , style "border" "1px solid lightgray"
                    , style "border-radius" "3px"
                    , onClick <| clickHandler item
                    ]
                    [ viewItem item ]
            )
