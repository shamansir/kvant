module Render.Flat exposing (..)

import Dict

import Html exposing (..)
import Html.Attributes exposing (..)

import Kvant.Plane exposing (..)
import Kvant.Plane.Flat exposing (..)
import Kvant.Plane.Flat as Plane exposing (sub ,subAt, foldMap, unpack)
import Kvant.Plane.Offset exposing (Offset, OffsetPlane)
import Kvant.Plane.Offset as Offsets exposing (foldMap)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane, TracingCell)
import Kvant.Matches as Matches exposing (..)
import Kvant.Solver as Solver
import Kvant.Solver.Flat as Solver
import Kvant.Vec2 exposing (..)
import Kvant.Occurrence exposing (frequencyToFloat)

import Render.Core as Render exposing (..)
import Render.Grid as Render exposing (..)


coordText : Vec2 -> String
coordText ( x, y ) = String.fromInt x ++ "," ++ String.fromInt y


coord : Vec2 -> Html msg
coord c =
    span
        [ style "position" "absolute"
        , style "font-size" "7px"
        , style "background-color" "lightgray"
        , style "padding" "2px"
        , style "border-radius" "7px"
        , style "opacity" "0.5"
        ]
        [ text <| "(" ++ coordText c ++ ")" ]


plane : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
plane default viewElem =
    Plane.unpack
        >> List.map (List.map <| Maybe.withDefault default)
        >> Render.grid viewElem


planeV : a -> (Vec2 -> a -> Html msg) -> Plane Vec2 a -> Html msg
planeV default viewElem =
    Plane.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default |> Tuple.pair v)
        >> Render.gridV viewElem


offsetPlane : a -> (a -> Html msg) -> OffsetPlane Vec2 a -> Html msg
offsetPlane default viewElem =
    Offsets.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default)
        >> grid viewElem


offsetPlaneV : a -> (Vec2 -> a -> Html msg) -> OffsetPlane Vec2 a -> Html msg
offsetPlaneV default viewElem =
    Offsets.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default |> Tuple.pair v)
        >> gridV viewElem


labeledList : a -> (Vec2 -> a -> Html msg) -> List ( String, Plane Vec2 a ) -> Html msg
labeledList default viewElem  =
    Render.listBy
        (\( label, thePlane ) ->
            div []
                [ text label
                , thePlane |> planeV default viewElem
                ]
        )


indexedList : a -> (Vec2 -> a -> Html msg) -> List (Plane Vec2 a) -> Html msg
indexedList default viewElem planes =
    labeledList default viewElem (planes |> List.indexedMap (String.fromInt >> Tuple.pair))


pattern
    :  a
    -> (Vec2 -> a -> Html msg)
    -> Solver.UniquePatterns Vec2 a
    -> Int
    -> Solver.PatternWithStats Vec2 a
    -> Html msg
pattern default viewElem uniquePatterns index patternWithStats =
     div
        [ class <| "pattern-" ++ String.fromInt index
        , style "margin" "10px 0"
        ]
        [ span [] [ text <| String.fromInt index ++ ". " ]
        , span [] [ text <| (Render.occursText <| Tuple.first patternWithStats.frequency) ++ ". " ]
        , span [] [ text <| "frequency: " ++
            (case Tuple.second patternWithStats.frequency of
                Just freq -> freq |> frequencyToFloat |> String.fromFloat
                Nothing -> "unknown") ]
        , patternWithStats.pattern |> planeV default viewElem
        , span [] [ text <| "Matches: " ]
        -- , viewMatches matches
        , patternWithStats.matches
            |> matchesWithPatterns default viewElem uniquePatterns
        ]


matches : OffsetPlane Vec2 (List Int) -> Html msg
matches thePlane =
    let
        itemSpan =
            span
                [ style "padding" "3px"
                ]
    in
        thePlane
            |> offsetPlane []
                (\matchesList ->
                    div
                        [ style "height" "50px"
                        , style "width" "50px"
                        , style "border" "1px dashed gray"
                        , style "display" "flex"
                        , style "flex-wrap" "wrap"
                        , style "font-size" "9px"
                        ]
                        <| List.map
                            (String.fromInt
                                >> text
                                >> List.singleton
                                >> itemSpan)
                            matchesList
                )


matchesWithPatterns
    :  a
    -> (Vec2 -> a -> Html msg)
    -> Solver.UniquePatterns Vec2 a
    -> OffsetPlane Vec2 (List Int)
    -> Html msg
matchesWithPatterns default viewElem uniquePatterns thePlane =
    let
        patternWrapper patternId patternData =
            div
                [ style "display" "inline-block"
                , style "padding" "1px"
                , style "transform" "scale(0.4,0.6)"
                , style "border" "1px solid"
                , style "max-width" "50px"
                , style "min-width" "50px"
                , style "margin-right" "-20px"
                ]
                [ patternData.pattern |> planeV default viewElem
                , span
                    [ style "position" "absolute"
                    , style "padding-top" "5px"
                    , style "font-size" "1.3em"
                    ]
                    [ text <| String.fromInt patternId ]
                ]
    in
        thePlane
            |> offsetPlaneV []
                (\theCoord matchesList ->
                    div [ style "height" "130px"
                        , style "width" "130px"
                        , style "border" "1px dashed gray"
                        ]
                        [ coord theCoord
                        , div
                            [ style "display" "flex"
                            , style "flex-direction" "row"
                            , style "align-items" "start"
                            , style "flex-wrap" "wrap"
                            , style "font-size" "9px"
                            ]
                            <| List.map
                                (\match ->
                                    uniquePatterns
                                        |> Dict.get match
                                        |> Maybe.map (patternWrapper match)
                                        |> Maybe.withDefault (div [] [ text "<NO>" ])
                                )
                                matchesList
                        ]
                )


tracing
    :  a
    -> (a -> Html msg)
    -> (Vec2 -> Html msg)
    -> TracingPlane Vec2 a
    -> Html msg
tracing contradiction viewElem viewCoord tracingPlane =
    tracingPlane
        |> planeV (Matches.none, []) (\theCoord theTracingCell ->
            span
                [ style "padding" "3px"
                , style "border" "1px dotted lightgray"
                ]
                [ viewCoord theCoord
                , Render.tracingCell
                    contradiction
                    viewElem
                    theTracingCell
                ])


tracingTiny
    :  a
    -> (Float -> a -> Html msg)
    -> (Vec2 -> Html msg)
    -> TracingPlane Vec2 a
    -> Html msg
tracingTiny default viewScaled viewCoord tracingPlane =
    let
        tinyTracingCell : TracingCell a -> Html msg
        tinyTracingCell ( _, items ) =
            span
                [ style "display" "inline-block"
                , style "width" "30px"
                , style "height" "30px"
                , style "overflow" "hidden"
                ]
                [
                    (
                        Render.asGrid
                            default
                            viewScaled
                            items
                    )
                ]
    in
        div
            -- [ style "position" "absolute"
            -- , style "right" "400px"
            -- , style "margin-top" "-400px"
            [ style "padding" "12px"
            , style "background" "rgba(255,255,255,0.95)"
            ]
            [ tracingPlane
                |> planeV (Matches.none, []) (\theCoord theTracingCell ->
                    span
                        [ style "border" "1px dotted rgba(255,255,255,0.1)"
                        ]
                        [ viewCoord theCoord
                        , tinyTracingCell theTracingCell
                        ])
            ]
