module Main exposing (..)


import Browser

import Random
import Task
import Time
-- import Dict
import Dict exposing (Dict)
import Dict as D exposing (map)
import Array exposing (Array)
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
    , expands : Array BlockState
    }


type Msg
    = NoOp
    | TriggerRunning
    | TriggerTracing
    | TriggerPreviousStep
    | Run Random.Seed
    | Trace Random.Seed
    | NextStep
    | PreviousStep Random.Seed
    | Stop
    | SwitchBlock Int



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
            Array.empty
        |> initExpands


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
            , makeSeedAnd Run
            )

        TriggerTracing ->
            (
                { model
                | status = Preparation
                }
            , makeSeedAnd Trace
            )

        TriggerPreviousStep ->
            ( model
            , makeSeedAnd PreviousStep
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

        PreviousStep newSeed ->
            ( case model.status of
                    Solving ( prevResult, prevTracingResult) history ->
                        -- TODO: Will be integrated into the Solver
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
                                    H.push
                                        ( lastStep |> WFC.changeSeedTo newSeed
                                        , TracingStep (lastTracingStep |> WFC.changeSeedTo newSeed)
                                        )
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

        SwitchBlock index ->
            (
                { model
                | expands =
                    model.expands |> switchBlock index
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


makeSeedAnd : (Random.Seed -> msg) -> Cmd msg
makeSeedAnd makeMsg =
    Task.perform
        (\time ->
            makeMsg <| Random.initialSeed <| Time.posixToMillis time
        )
        Time.now


type Block
    = Source String Vec2
    | RunOnce Status Vec2
    | Tracing Status
    | RotationsAndFlips TextPlane
    | SubPlanes TextPlane
    | PeriodicSubPlanes TextPlane
    | AllViews TextPlane
    | Patterns TextPlane
    | AllSubPlanes TextPlane SearchMethod (N Vec2)
    | Empty


type BlockState
    = Expanded
    | Collapsed


blocks : Model -> List Block
blocks model =
    [ Source model.source options.inputSize
    , RunOnce model.status options.outputSize
    , Tracing model.status
    , RotationsAndFlips testPlane
    , RotationsAndFlips testPlaneHex
    , SubPlanes testPlane
    , PeriodicSubPlanes testPlane
    , SubPlanes testPlaneHex
    , PeriodicSubPlanes testPlaneHex
    , AllViews testPlane
    , AllViews testPlaneHex
    , Patterns testPlane
    , Patterns testPlaneHex
    , AllSubPlanes testPlane options.patternSearch options.patternSize
    , AllSubPlanes testPlaneHex options.patternSearch options.patternSize
    ]


initExpands : Model -> Model
initExpands model =
    { model
    | expands =
        Array.initialize
            (List.length <| blocks model)
            (always Expanded)
    }


switchBlock : Int -> Array BlockState -> Array BlockState
switchBlock index blocksArr =
    case blocksArr
        |> Array.get index of
        Just state ->
            blocksArr
                |> Array.set index
                    (case state of
                        Collapsed -> Expanded
                        Expanded -> Collapsed
                        )
        Nothing -> blocksArr


blockTitle : Block -> String
blockTitle block =
    case block of
        Source _ _ -> "Source"
        RunOnce _ _ -> "Run"
        Tracing _ -> "Trace"
        RotationsAndFlips _ -> "Rotations and Flips"
        SubPlanes _ -> "SubPlanes"
        PeriodicSubPlanes _ -> "Periodic SubPlanes"
        AllViews _ -> "Views"
        Patterns _ -> "Patterns"
        AllSubPlanes _ _ _ -> "All Possible SubPlanes"
        Empty -> "?"


viewBlock : Block -> Html Msg
viewBlock block =
    case block of

        Source source bounds ->
            source |> viewTextInBounds bounds

        RunOnce status bounds ->
            div []
                [ button
                    [ onClick TriggerRunning
                    , disabled <| doingSomething status
                    ]
                    [ text "Run once" ]
                , status
                    |> getCurrentPlane
                    |> Maybe.map Tuple.first
                    |> Maybe.map (viewTextInBounds bounds)
                    |> Maybe.withDefault (div [] [])
                ]

        Tracing status ->
            div []
                [ button
                    [ onClick TriggerTracing
                    , disabled <| doingSomething status
                    ]
                    [ text "Start tracing" ]
                , button
                    [ onClick TriggerPreviousStep
                    , disabled <| status == None
                    ]
                    [ text "Previous Step" ]
                , button
                    [ onClick NextStep
                    , disabled <| status == None
                    ]
                    [ text "Next Step" ]
                , status
                    |> getHistory
                    |> Maybe.map H.last
                    |> Maybe.map Tuple.first -- Tuple.second?
                    |> Maybe.map viewStepStatus
                    |> Maybe.withDefault (span [] [])
                , button
                    [ onClick Stop
                    , disabled <| status == None
                    ]
                    [ text "Stop" ]
                , status
                    |> getCurrentPlane
                    |> Maybe.map Tuple.second
                    |> Maybe.map viewTracingPlane
                    |> Maybe.withDefault (div [] [])
                , status
                    |> getCurrentPlane
                    |> Maybe.map Tuple.second
                    |> Maybe.map viewTinyTracingPlane
                    |> Maybe.withDefault (div [] [])
                , status
                    |> getHistory
                    |> Maybe.map (H.map Tuple.second >> H.map unpackTracingStep)
                    |> Maybe.map viewHistory
                    |> Maybe.withDefault (div [] [])
                ]

        RotationsAndFlips plane ->
            div []
                [ plane |> viewMaterialized
                , hr [] []
                , plane |> rotate |> viewMaterialized
                , hr [] []
                , plane |> rotate |> flip |> viewMaterialized
                , hr [] []
                , plane |> viewRotationsAndFlips
                ]

        SubPlanes plane ->
            plane |> viewSubPlanes

        PeriodicSubPlanes plane ->
            plane |> viewPeriodicSubPlanes

        AllViews plane ->
            plane |> viewAllViews

        Patterns plane ->
            plane |> viewPatterns options.patternSearch options.patternSize

        AllSubPlanes plane patternSearch patternSize ->
            plane |> viewAllSubPlanes patternSearch patternSize

        Empty -> div [] []


view : Model -> Html Msg
view model =
    div
        [ ]
        (blocks model
            |> List.indexedMap
                (\index block ->
                    div []
                        [
                            span
                                [ style "cursor" "pointer"
                                , onClick <| SwitchBlock index
                                ]
                                [ text <| blockTitle block
                                ]
                        , case
                            Array.get index model.expands
                                |> Maybe.withDefault Expanded of
                            Expanded -> viewBlock block
                            Collapsed -> text "..."
                        ]
                )
            |> List.intersperse (hr [] []))


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


