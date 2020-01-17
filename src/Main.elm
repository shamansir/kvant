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
import WFC.Plane.Impl.Tracing exposing (TracingPlane)
import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Plane, N(..))
import WFC.Plane.Flat as Plane exposing (SearchMethod(..))
import WFC.Plane.Flat exposing (flip, rotate)
import WFC.Plane.Impl.Text exposing (TextPlane)
import WFC.Plane.Impl.Text as TextPlane exposing (make)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step, Options)
import WFC.Solver.History as H exposing (..)

type TracingStep v = TracingStep (WFC.Step v)
type alias History v = H.History (WFC.Step v, TracingStep v)


type Status v fmt a
    = None
    | Preparation
    | Solving ( fmt, TracingPlane v a ) (History v)
    | Solved ( fmt, TracingPlane v a )


type alias Model =
    { examples: List Example
    }


type ExampleMsg
    = TriggerRunning
    | TriggerTracing
    | TriggerPreviousStep
    | Run Random.Seed
    | Trace Random.Seed
    | NextStep
    | PreviousStep Random.Seed
    | Stop
    | SwitchBlock Int


type alias ExampleId = Int

type Msg
    = NoOp
    | WithExample ExampleId ExampleMsg


textOptions : WFC.Options Vec2
textOptions =
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
        quickTextExample src size =
            TextExample
                (
                    { source = src
                    , sourcePlane = TextPlane.make size src
                    , options = textOptions
                    , expands = []
                    , wfc =
                        ( WFC.text textOptions src
                        , WFC.textTracing textOptions src
                        )
                    , status = None
                    }
                |> initExpands)
    in
        { examples =
            [ quickTextExample
                (
                    "AAAA" ++
                    "ABBA" ++
                    "ABBA" ++
                    "AAAA"
                )
                (4, 4)
            , quickTextExample
                (
                    "0000" ++
                    "0111" ++
                    "0121" ++
                    "0111"
                )
                (4, 4)
            , quickTextExample
                (
                    "0123" ++
                    "4567" ++
                    "89AB" ++
                    "CDEF"
                )
                (4, 4)
            ]
        }


update
    :  Msg
    -> Model
    -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WithExample requestedId exampleMsg ->
            let
                examplesUpdates
                    = model.examples
                        |> List.indexedMap
                                (\index example ->
                                    if index == requestedId then
                                        case example of
                                            TextExample textModel ->
                                                let
                                                    (m, c) =
                                                        updateExample requestedId exampleMsg textModel
                                                in
                                                    ( TextExample m
                                                    , c |> Cmd.map (WithExample index)
                                                    )
                                    else
                                        ( example, Cmd.none )
                                )
            in
                (
                    { model
                    | examples = examplesUpdates |> List.map Tuple.first
                    }
                , examplesUpdates
                    |> List.map Tuple.second
                    |> Cmd.batch
                )


updateExample
    :  ExampleId
    -> ExampleMsg
    -> ExampleModel v fmt a
    -> ( ExampleModel v fmt a, Cmd ExampleMsg )
updateExample id msg model =
    case msg of

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


doingSomething : Status v fmt a -> Bool
doingSomething status =
    case status of
        None -> False
        _ -> True


-- getExampleModel : Example -> ExampleModel v fmt a
-- getExampleModel example =
--     case example of
--        TextExample model -> model


getCurrentPlane : Status v fmt a -> Maybe ( fmt, TracingPlane v a )
getCurrentPlane status =
    case status of
        Solving plane _ -> Just plane
        Solved plane -> Just plane
        _ -> Nothing


getHistory : Status v fmt a -> Maybe (History v)
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


type Block v fmt a
    = Source v fmt
    | RunOnce v (Status v fmt a)
    | Tracing (Status v fmt a)
    | RotationsAndFlips (Plane v a)
    | SubPlanes (Plane v a)
    | PeriodicSubPlanes (Plane v a)
    | AllViews (Plane v a)
    | Patterns (Plane v a) SearchMethod (N v)
    | AllSubPlanes (Plane v a) SearchMethod (N v)
    | Empty


type BlockState
    = Expanded
    | Collapsed


type alias TextBlock = Block Vec2 String Char


type alias ExampleModel v fmt a =
    { source : fmt
    , sourcePlane : Plane v a
    , options : WFC.Options v
    , expands : List BlockState
    , wfc : ( WFC v fmt a, WFC.TracingWFC v a )
    , status : Status v fmt a
    }


type Example
    = TextExample (ExampleModel Vec2 String Char)


initExpands : ExampleModel v fmt a -> ExampleModel v fmt a
initExpands exampleModel =
    { exampleModel
    | expands =
        blocks exampleModel
            |> List.map
                (\block ->
                    case block of
                        Source _ _ -> Expanded
                        RunOnce _ _-> Expanded
                        Tracing _ -> Expanded
                        _ -> Collapsed
                )
    }


blocks : ExampleModel v fmt a -> List (Block v fmt a)
blocks e =
    [ Source e.options.inputSize e.source
    , RunOnce e.options.outputSize e.status
    , Tracing e.status
    , RotationsAndFlips e.sourcePlane
    , SubPlanes e.sourcePlane
    , PeriodicSubPlanes e.sourcePlane
    , AllViews e.sourcePlane
    , Patterns
            e.sourcePlane
            e.options.patternSearch
            e.options.patternSize
    , AllSubPlanes
            e.sourcePlane
            e.options.patternSearch
            e.options.patternSize
    ]


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


blockTitle : Block v fmt a -> String
blockTitle block =
    case block of
        Source _ _ -> "Source"
        RunOnce _ _ -> "Run"
        Tracing _ -> "Trace"
        RotationsAndFlips _ -> "Rotations and Flips"
        SubPlanes _ -> "SubPlanes"
        PeriodicSubPlanes _ -> "Periodic SubPlanes"
        AllViews _ -> "Views"
        Patterns _ _ _ -> "Patterns"
        AllSubPlanes _ _ _ -> "All Possible SubPlanes"
        Empty -> "?"


viewBlock : TextBlock -> Html ExampleMsg
viewBlock block =
    case block of

        Source bounds source ->
            source |> viewTextInBounds bounds

        RunOnce bounds status ->
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

        Patterns plane patternSearch patternSize ->
            plane |> viewPatterns patternSearch patternSize

        AllSubPlanes plane patternSearch patternSize ->
            plane |> viewAllSubPlanes patternSearch patternSize

        Empty -> div [] []



-- getBlocks : Example -> List (BlockState, Block Vec2 String Char)
-- getBlocks example =
--     case example of
--         TextExample exampleModel -> exampleModel.blocks


getModel : Example -> ExampleModel Vec2 String Char
getModel example =
    case example of
        TextExample exampleModel -> exampleModel


view : Model -> Html Msg
view model =
    div
        [ ]
        (model.examples
            |> List.map getModel
            |> List.indexedMap (\exampleId example ->
                div []
                    (blocks example
                        |> List.map2 Tuple.pair example.expands
                        |> List.indexedMap
                            (\index ( isExpanded, block ) ->
                                div []
                                    [
                                        span
                                            [ style "cursor" "pointer"
                                            , onClick
                                                <| WithExample exampleId
                                                <| SwitchBlock index
                                            ]
                                            [ text <| blockTitle block
                                            ]
                                    , case isExpanded of
                                        Expanded ->
                                            viewBlock block
                                                |> Html.map (WithExample exampleId)
                                        Collapsed -> text "..."
                                    ]
                            )
                        |> List.intersperse (hr [] [])
                    )
                )
        )


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


