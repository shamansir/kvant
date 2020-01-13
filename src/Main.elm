module Main exposing (..)


import Browser

import Random
import Task
import Time
-- import Dict
import Dict exposing (Dict)
import Dict as D exposing (map)
import Array as A exposing (map)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import View exposing (..)

import WFC.Core exposing (WFC, TextWFC, TextTracingWFC, TextTracingPlane)
import WFC.Core as WFC
import WFC.Vec2 exposing (..)
import WFC.Plane exposing (N(..))
import WFC.Plane.Flat as Plane exposing (SearchMethod(..))
import WFC.Plane.Flat exposing (flip, rotate)
import WFC.Plane.Impl.Text exposing (TextPlane)
import WFC.Plane.Impl.Text as TextPlane exposing (make)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step, Options)
import WFC.Solver.History as H exposing (..)

type TracingStep v = TracingStep (WFC.Step v)
type alias History v = H.History (WFC.Step v, TracingStep v)


type Status
    = None
    | Preparation
    | Solving ( String, TextTracingPlane ) (History Vec2)
    | Solved ( String, TextTracingPlane )


type alias Model =
    { source: String
    , wfc : ( TextWFC, TextTracingWFC )
    , status : Status
    }


type Msg
    = NoOp
    | TriggerRunning
    | TriggerTracing
    | Run Random.Seed
    | Trace Random.Seed
    | NextStep
    | PreviousStep
    | Stop



options : WFC.Options Vec2
options =
    { approach = Overlapping
    , patternSearch = Bounded -- Periodic
    , patternSize = N ( 2, 2 )
    , inputSize = ( 4, 4 )
    , outputSize = ( 10, 10 )
    -- , advanceRule = WFC.MaximumAttempts 50
    , advanceRule = WFC.AdvanceManually
    }


init : Model
init =
    let
        srcText =
            (
                "0000" ++
                "0111" ++
                "0121" ++
                "0111"
            )
    in
        Model
            srcText
            ( WFC.text options srcText
            , WFC.textTracing options srcText
            )
            None


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        NoOp ->
            ( model, Cmd.none )

        TriggerRunning ->
            (
                { model
                | status = Preparation
                }
            , Task.perform
                (\time ->
                    Run <| Random.initialSeed <| Time.posixToMillis time
                )
                Time.now
            )

        TriggerTracing ->
            (
                { model
                | status = Preparation
                }
            , Task.perform
                (\time ->
                    Trace <| Random.initialSeed <| Time.posixToMillis time
                )
                Time.now
            )

        Run seed ->
            (
                { model
                | status = Solved
                    ( model.wfc
                        |> Tuple.mapBoth
                                (WFC.run seed)
                                (WFC.run seed)
                    )
                }
            , Cmd.none
            )

        Trace seed ->
            (
                let
                    (lastStep, result) =
                        model.wfc |> Tuple.first |> WFC.Core.firstStep seed
                    (lastTracingStep, tracingResult) =
                        model.wfc |> Tuple.second |> WFC.Core.firstStep seed
                in
                    { model
                    | status =
                        Solving
                            ( result, tracingResult )
                            <| Debug.log "history"
                            <| H.init (lastStep, TracingStep lastTracingStep)
                    }
            , Cmd.none
            )

        NextStep ->
            (
                case model.status of
                    Solving _ history ->
                        let
                            (step, TracingStep tracingStep) = H.last history
                            (lastStep, result) =
                                model.wfc |> Tuple.first |> WFC.step step
                            (lastTracingStep, tracingResult) =
                                model.wfc |> Tuple.second |> WFC.step tracingStep
                            nextHistory =
                                history |>
                                    H.push ( lastStep, TracingStep lastTracingStep )
                        in
                            { model
                            | status =
                                Solving
                                    ( result, tracingResult )
                                    nextHistory
                            }
                    _ -> model
            , Cmd.none
            )

        PreviousStep ->
            ( case model.status of
                    Solving ( prevResult, prevTracingResult) history ->
                        let
                            -- we need to remove two last steps from the history
                            -- to re-run it again from the start and then repeat
                            -- the step which was before the latest, to know
                            -- which result was there at this point
                            historyAStepBack = history |> H.back |> H.back
                            (lastStep, result) =
                                model.wfc
                                    |> Tuple.first
                                    |> WFC.stepAtOnce
                                        (H.toList historyAStepBack
                                            |> List.map Tuple.first)
                                    |> Maybe.withDefault
                                        (H.last history
                                            |> Tuple.first
                                            |> \s -> ( s, prevResult ))
                            (lastTracingStep, tracingResult) =
                                model.wfc
                                    |> Tuple.second
                                    |> WFC.stepAtOnce
                                        (H.toList historyAStepBack
                                            |> List.map Tuple.second
                                            |> List.map (\(TracingStep s) -> s))
                                    |> Maybe.withDefault
                                        (H.last history
                                            |> Tuple.second
                                            |> (\(TracingStep s) -> s)
                                            |> \s -> ( s, prevTracingResult ))
                            nextHistory =
                                historyAStepBack |>
                                    H.push ( lastStep, TracingStep lastTracingStep )
                        in
                            { model
                            | status =
                                Solving
                                    ( result, tracingResult )
                                    nextHistory
                            }
                    _ -> model
            , Cmd.none
            )

        Stop ->
            (
                { model
                | status = None
                }
            , Cmd.none )


testPlane : TextPlane
testPlane =
    TextPlane.make (4, 4)
        (
            "0000" ++
            "0111" ++
            "0121" ++
            "0111"
        )


testPlaneHex : TextPlane
testPlaneHex =
    TextPlane.make (4, 4)
        (
            "0123" ++
            "4567" ++
            "89AB" ++
            "CDEF"
        )


doingSomething : Status -> Bool
doingSomething status =
    case status of
        None -> False
        _ -> True


getCurrentPlane : Status -> Maybe ( String, TextTracingPlane )
getCurrentPlane status =
    case status of
        Solving plane _ -> Just plane
        Solved plane -> Just plane
        _ -> Nothing


getHistory : Status -> Maybe (History Vec2)
getHistory status =
    case status of
        Solving plane history -> Just history
        _ -> Nothing


unpackTracingStep : TracingStep Vec2 -> Step Vec2
unpackTracingStep (TracingStep step) = step


view : Model -> Html Msg
view model =
    div
        [ ]
        [ model.source
            |> viewTextInBounds options.inputSize
        -- , text <| if model.status /= None then  "Busy" else "Ready"
        , hr [] []
        , button
            [ onClick TriggerRunning
            , disabled <| doingSomething model.status
            ]
            [ text "Run once" ]
        , model.status
            |> getCurrentPlane
            |> Maybe.map Tuple.first
            |> Maybe.map (viewTextInBounds options.outputSize)
            |> Maybe.withDefault (div [] [])
        , hr [] []
        , button
            [ onClick TriggerTracing
            , disabled <| doingSomething model.status
            ]
            [ text "Start tracing" ]
        , button
            [ onClick PreviousStep
            , disabled <| model.status == None
            ]
            [ text "Previous Step" ]
        , button
            [ onClick NextStep
            , disabled <| model.status == None
            ]
            [ text "Next Step" ]
        , model.status
            |> getHistory
            |> Maybe.map H.last
            |> Maybe.map Tuple.first -- Tuple.second?
            |> Maybe.map viewStepStatus
            |> Maybe.withDefault (span [] [])
        , button
            [ onClick Stop
            , disabled <| model.status == None
            ]
            [ text "Stop" ]
        , model.status
            |> getCurrentPlane
            |> Maybe.map Tuple.second
            |> Maybe.map viewTracingPlane
            |> Maybe.withDefault (div [] [])
        , model.status
            |> getHistory
            |> Maybe.map (H.map Tuple.second >> H.map unpackTracingStep)
            |> Maybe.map viewHistory
            |> Maybe.withDefault (div [] [])
        -- --------------------------
        , hr [] []
        , hr [] []
        , hr [] []
        , testPlaneHex |> viewMaterialized
        , hr [] []
        , testPlaneHex |> rotate |> viewMaterialized
        , hr [] []
        , testPlaneHex |> rotate |> flip |> viewMaterialized
        , hr [] []
        , testPlane |> viewRotationsAndFlips
        , hr [] []
        , testPlaneHex |> viewRotationsAndFlips
        , hr [] []
        , testPlane |> viewSubPlanes
        , hr [] []
        , testPlaneHex |> viewSubPlanes
        , hr [] []
        , testPlane |> viewPeriodicSubPlanes
        , hr [] []
        , testPlaneHex |> viewPeriodicSubPlanes
        , hr [] []
        , testPlane |> viewAllViews
        , hr [] []
        , testPlaneHex |> viewAllViews
        , hr [] []
        , testPlane
            |> viewPatterns options.patternSearch options.patternSize
        , hr [] []
        , testPlaneHex
            |> viewPatterns options.patternSearch options.patternSize
        , hr [] []
        , testPlane
            |> viewAllSubPlanes options.patternSearch options.patternSize
        , hr [] []
        , testPlaneHex
            |> viewAllSubPlanes options.patternSearch options.patternSize
        ]


main : Program {} Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( init, Cmd.none )
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "WFC", body = [ view model ] }
        }


