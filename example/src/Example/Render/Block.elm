module Example.Render.Block exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Example.Advance exposing (..)
import Example.Render.Renderer exposing (Renderer)
import Example.Msg exposing (Msg(..))

import Kvant.Plane exposing (Plane, N)
import Kvant.Plane.Flat exposing (Boundary)
import Kvant.Solver.History as H exposing (map)


type Block v fmt a
    = Source fmt
    | RunOnce v (Status v fmt a)
    | Tracing (Status v fmt a)
    | RotationsAndFlips (Plane v a)
    | SubPlanes (Plane v a)
    | PeriodicSubPlanes (Plane v a)
    | AllViews (Plane v a)
    | Patterns (Plane v a) Boundary (N v)
    | AllSubPlanes (Plane v a) Boundary (N v)
    | Empty


type BlockState
    = Expanded
    | Collapsed


viewBlock : Renderer v fmt a Msg -> Block v fmt a -> Html Msg
viewBlock render block =
    case block of

        Source source ->
            source |> render.source -- |> Html.map (always NoOp)
                -- viewTextInBounds bounds

        RunOnce bounds status ->
            div []
                [ button
                    [ onClick TriggerRunning
                    , disabled <| doingSomething status
                    ]
                    [ Html.text "Run once" ]
                , status
                    |> getCurrentPlane
                    |> Maybe.map Tuple.first
                    |> Maybe.map render.source
                    |> Maybe.withDefault (div [] [])
                ]

        Tracing status ->
            div []
                [ button
                    [ onClick TriggerTracing
                    , disabled <| doingSomething status
                    ]
                    [ Html.text "Start tracing" ]
                , button
                    [ onClick TriggerPreviousStep
                    , disabled <| status == None
                    ]
                    [ Html.text "Previous Step" ]
                , button
                    [ onClick NextStep
                    , disabled <| status == None
                    ]
                    [ Html.text "Next Step" ]
                , status
                    |> getHistory
                    |> Maybe.map H.last
                    |> Maybe.map Tuple.first -- Tuple.second?
                    |> Maybe.map render.step
                    |> Maybe.withDefault (span [] [])
                , button
                    [ onClick Stop
                    , disabled <| status == None
                    ]
                    [ Html.text "Stop" ]
                , status
                    |> getCurrentPlane
                    |> Maybe.map Tuple.first
                    |> Maybe.map render.source
                    |> Maybe.withDefault (div [] [])
                , status
                    |> getCurrentPlane
                    |> Maybe.map Tuple.second
                    |> Maybe.map render.tracing
                    |> Maybe.withDefault (div [] [])
                , status
                    |> getCurrentPlane
                    |> Maybe.map Tuple.second
                    |> Maybe.map render.tracingTiny
                    |> Maybe.withDefault (div [] [])
                , status
                    |> getHistory
                    |> Maybe.map (H.map Tuple.second >> H.map unpackTracingStep)
                    |> Maybe.map render.history
                    |> Maybe.withDefault (div [] [])
                ]

        RotationsAndFlips plane ->
            div []
                {-
                [ plane |> viewMaterialized
                , hr [] []
                , plane |> rotate |> viewMaterialized
                , hr [] []
                , plane |> rotate |> flip |> viewMaterialized
                , hr [] []
                -}
                [ plane |> render.rotationsAndFlips
                ]

        SubPlanes plane ->
            plane |> render.subPlanes

        PeriodicSubPlanes plane ->
            plane |> render.periodicSubPlanes

        AllViews plane ->
            plane |> render.allViews

        Patterns plane patternSearch patternSize ->
            plane |> render.patterns patternSearch patternSize

        AllSubPlanes plane patternSearch patternSize ->
            plane |> render.allSubPlanes patternSearch patternSize

        Empty -> div [] []


title : Block v fmt a -> String
title block =
    case block of
        Source _ -> "Source"
        RunOnce _ _ -> "Run"
        Tracing _ -> "Trace"
        RotationsAndFlips _ -> "Rotations and Flips"
        SubPlanes _ -> "SubPlanes"
        PeriodicSubPlanes _ -> "Periodic SubPlanes"
        AllViews _ -> "Views"
        Patterns _ _ _ -> "Patterns"
        AllSubPlanes _ _ _ -> "All Possible SubPlanes"
        Empty -> "?"


switchBlock : Int -> List BlockState -> List BlockState
switchBlock index states =
    states
        |> List.indexedMap
            (\blockIndex expandState ->
                if index == blockIndex then
                    case expandState of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                else expandState
            )
