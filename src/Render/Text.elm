module Render.Text exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)

import Render.Core as Render exposing (..)
import Render.Grid as Render exposing (..)
import Render.Flat as Render exposing (..)

import WFC.Vec2 exposing (..)
import WFC.Matches as Matches exposing (..)
import WFC.Plane exposing (Cell)
import WFC.Plane.Impl.Text exposing (..)
import WFC.Plane.Impl.Tracing exposing (..)


splitBy : Int -> String -> List String
splitBy width src =
    let
        next = src |> String.left width
        left = src |> String.dropLeft width
    in
        if String.length left > 0 then
            next :: splitBy width left
        else
            [ next ]


char : Char -> Html msg
char c =
    span
        [ style "display" "inline-block"
        , style "width" "9px"
        , style "background-color" <| symbolBg c
        , style "padding" "2px 8px"
        , style "color" <|
            if symbolBg c == "black" then
                "rgba(255,255,255,0.3)"
            else
                "rgba(0,0,0,0.3)"
        ]
        [ text <| String.fromChar c ]


symbolBg : Char -> String
symbolBg symbol =
    case symbol of
        '0' -> "white"
        '1' -> "black"
        '2' -> "red"
        '3' -> "aqua"
        '4' -> "blue"
        '5' -> "green"
        '6' -> "teal"
        '7' -> "maroon"
        '8' -> "#85C1E9"
        '9' -> "#5D6D7E"
        'A' -> "#5DADE2"
        'B' -> "#F9E79F"
        'C' -> "#F4D03F"
        'D' -> "#E74C3C"
        'E' -> "#BFC9CA"
        'F' -> "#2E86C1"
        'x' -> "salmon"
        _ -> "lightgray"


viewTextInBounds : Vec2 -> String -> Html msg
viewTextInBounds (width, height) string =
    string
        |> splitBy width
        |> List.map (String.toList)
        |> Render.grid char


viewTextPlane : TextPlane -> Html msg
viewTextPlane =
    Render.withCoords '?' char


viewTracingPlane : TracingPlane Vec2 Char -> Html msg
viewTracingPlane =
    Render.planeV (Matches.none, [])
        <| \coord tracingCell ->
            span
                [ style "padding" "3px"
                , style "border" "1px dotted lightgray"
                ]
                [ Render.coord coord
                , viewTracingCell tracingCell
                ]


viewTinyTracingPlane : TracingPlane Vec2 Char -> Html msg
viewTinyTracingPlane plane =
    div
        [ style "position" "absolute"
        , style "right" "400px"
        , style "margin-top" "-400px"
        , style "padding" "12px"
        , style "background" "rgba(255,255,255,0.95)"
        ]
        [
            Render.planeV (Matches.none, [])
                (\coord tracingCell ->
                    span
                        [ style "border" "1px dotted rgba(255,255,255,0.1)"
                        ]
                        [ Render.coord coord
                        , viewTinyTracingCell tracingCell
                        ])
                plane
        ]


viewCell : Cell Vec2 Char -> Html msg
viewCell =
    Render.cell '%' (text << String.fromChar)


viewTinyTracingCell : TracingCell Char -> Html msg
viewTinyTracingCell ( _, chars ) =
        span
            [ style "display" "inline-block"
            , style "width" "30px"
            , style "height" "30px"
            , style "overflow" "hidden"
            ]
            [
                (
                    Render.asGrid
                        'x'
                        (\scale theChar ->
                            span
                                [ style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
                                , style "width" "10px"
                                , style "height" "10px" ]
                                [ char theChar ]
                        )
                        chars
                )
            ]


viewTracingCell : TracingCell Char -> Html msg
viewTracingCell ( matches, chars ) =
    span
        [ ]
        [ span
            [ style "display" "inline-block"
            , style "width" "100px"
            , style "height" "88px"
            , style "padding-top" "12px"
            , style "overflow" "hidden"
            , style "text-overflow" "ellipsis"
            ]
            [
                Matches.toList matches
                    |> List.map String.fromInt
                    |> String.join "|"
                    |> text
            ]
        , span
            [ style "display" "inline-block"
            , style "width" "100px"
            , style "max-width" "100px"
            , style "height" "100px"
            , style "max-height" "100px"
            , style "overflow" "hidden"
            , style "text-overflow" "ellipsis"
            ]
            <| case List.length chars of
                    0 -> [ text "∅" ]
                    _ -> chars |> List.map char
        ]
