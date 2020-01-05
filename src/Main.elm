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


type TracingStep v = TracingStep (WFC.Step v)


type Status
    = None
    | Busy
    | Tracing (WFC.Step Vec2, TracingStep Vec2)


type alias Model =
    { source: String
    , current: Maybe ( String, TextTracingPlane )
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
            Nothing
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
                | current = Nothing
                , status = Busy
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
                | current = Nothing
                , status = Busy
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
                | current = Just
                    ( model.wfc
                        |> Tuple.mapBoth
                                (WFC.run seed)
                                (WFC.run seed)
                    )
                , status = None
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
                    | current = Just ( result, tracingResult )
                    , status = Tracing ( lastStep, TracingStep lastTracingStep )
                    }
            , Cmd.none
            )
        NextStep ->
            (
                case model.status of
                    Tracing ( step, TracingStep tracingStep ) ->
                        let
                            (lastStep, result) =
                                model.wfc |> Tuple.first |> WFC.step step
                            (lastTracingStep, tracingResult) =
                                model.wfc |> Tuple.second |> WFC.step tracingStep
                        in
                            { model
                            | current = Just ( result, tracingResult )
                            , status = Tracing ( lastStep, TracingStep lastTracingStep )
                            }
                    _ -> model
            , Cmd.none
            )
        Stop ->
            (
                { model
                -- | current = Nothing
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
        Busy -> True
        Tracing _ -> True


maybeStep : Status -> Maybe (WFC.Step Vec2)
maybeStep status =
    case status of
        None -> Nothing
        Busy -> Nothing
        Tracing ( step, _ ) -> Just step


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
        , model.current
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
            [ onClick NextStep
            , disabled <| model.status == None
            ]
            [ text "Next Step" ]
        , model.status |> maybeStep |> Maybe.map viewStepStatus |> Maybe.withDefault (span [] [])
        , button
            [ onClick Stop
            , disabled <| model.status == None
            ]
            [ text "Stop" ]
        , model.current
            |> Maybe.map Tuple.first
            |> Maybe.map (viewTextInBounds options.outputSize)
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


