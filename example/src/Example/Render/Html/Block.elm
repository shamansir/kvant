module Example.Render.Html.Block exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Example.Advance exposing (..)
import Example.Msg exposing (Msg(..))
import Example.Render.Renderer exposing (..)
import Example.Render.Block exposing (..)

import Kvant.Solver.History as H exposing (map)


viewBlock : HtmlRenderer v fmt a Msg -> Block v fmt a -> Html Msg
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
