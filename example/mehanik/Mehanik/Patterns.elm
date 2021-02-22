module Mehanik.Patterns exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (Array)
import Dict exposing (Dict)

import Kvant.Plane as Plane
import Kvant.Patterns exposing (AtomId, PatternId, Pattern)
import Kvant.Adjacency as Adjacency exposing (Adjacency)

import Mehanik.Grid exposing (..)
import Mehanik.Generic exposing (viewList)


applyWave : AtomId -> (Array AtomId -> b) -> Grid PatternId -> Adjacency Int Pattern -> Array (Array b)
applyWave default f wave adjacency =
    wave
        |> mapGrid
            (\patternId ->
                adjacency
                    |> Adjacency.get patternId
                    |> Maybe.andThen (Plane.get (0, 0))
                    |> Maybe.withDefault default
            )
        |> adaptGrid f


viewPatterns clickHandler viewPattern patterns =
    viewList
        clickHandler
        viewPattern
        <| Dict.toList
        <| patterns

    {-div
        [ style "display" "flex"
        , style "overflow" "scroll"
        ]
        <| List.map
            (\(patternId, { subject } ) ->
                div
                    [ style "transform" "scale(0.5)"
                    , style "margin" "5px"
                    , style "cursor" "pointer"
                    , style "padding" "3px"
                    , style "border" "1px solid lightgray"
                    , style "border-radius" "3px"
                    , onClick <| clickHandler patternId
                    ]
                    [ viewPattern patternId subject ]
            )
        <| Dict.toList
        <| patterns -}
