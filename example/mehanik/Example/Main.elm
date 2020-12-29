module Example.Main exposing (..)


import Random
import Task
import Time

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Color exposing (Color)
import Image exposing (Image)

import Kvant.Vec2 exposing (..)
import Kvant.Core as Wfc exposing (..)
import Kvant.Core as Core exposing (firstStep)
import Kvant.Plane exposing (Plane, N)
import Kvant.Plane.Flat exposing (Boundary)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Solver exposing (Step)
import Kvant.Solver as Solver exposing (Options)
import Kvant.Solver.History as H exposing (History)


import Example.Example exposing (Example)
import Example.Advance exposing (..)
import Example.Msg as Example exposing (Msg(..))
import Example.Render exposing (Renderer)


type alias ExampleId = Int


preview
    :  Renderer v fmt a (Html msg)
    -> fmt
    -> Html msg -- Example.Msg
preview renderer source =
    renderer.source source



view
    :  Renderer v fmt a (Html msg)
    -> Example v fmt a
    -> Html msg -- Example.Msg
view renderer example =
    div
        []
        [ renderer.plane example.sourcePlane
        , case example.status of
            None ->
                Html.text ""
            Preparation ->
                Html.text "Solving..."
            Solving ( fmt, tracingPlane ) history ->
                div
                    [ style "margin" "5px" ]
                    [ renderer.source fmt
                    , renderer.tracingPlane tracingPlane
                    ]
            Solved ( fmt, tracingPlane ) ->
                div
                    [ style "margin" "5px" ]
                    [ renderer.source fmt
                    , renderer.tracingPlane tracingPlane
                    ]
        ]


update
    :  Example.Msg
    -> Example v fmt a
    -> ( Example v fmt a, Cmd Example.Msg )
update msg model =
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

        TriggerFastForward ->
            ( model
            , makeSeedAnd Trace -- FIXME
            )

        Run seed ->
            (
                let
                    newWfc = model.makeWfc AtOnce
                in
                    { model
                    | wfc = newWfc
                    , status = Solved
                        ( newWfc
                            |> Tuple.mapBoth
                                    (Wfc.run seed)
                                    (Wfc.run seed)
                        )
                    }
            , Cmd.none
            )

        Trace seed ->
            (
                let
                    newWfc = model.makeWfc StepByStep
                    (lastStep, result) =
                        newWfc |> Tuple.first |> Core.firstStep seed
                    (lastTracingStep, tracingResult) =
                        newWfc |> Tuple.second |> Core.firstStep seed
                in
                    { model
                    | wfc = newWfc
                    , status =
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
                                model.wfc |> Tuple.first |> Wfc.step step
                            (lastTracingStep, tracingResult) =
                                model.wfc |> Tuple.second |> Wfc.step tracingStep
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
                                    |> Wfc.stepAtOnce
                                        (H.toList historyAStepBack
                                            |> List.map Tuple.first)
                                    |> Maybe.withDefault
                                        (H.last history
                                            |> Tuple.first
                                            |> \s -> ( s, prevResult ))
                            (lastTracingStep, tracingResult) =
                                model.wfc
                                    |> Tuple.second
                                    |> Wfc.stepAtOnce
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
                                        ( lastStep |> Solver.changeSeedTo newSeed
                                        , TracingStep
                                            (lastTracingStep |> Solver.changeSeedTo newSeed)
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


makeSeedAnd : (Random.Seed -> msg) -> Cmd msg
makeSeedAnd makeMsg =
    Task.perform
        (\time ->
            makeMsg <| Random.initialSeed <| Time.posixToMillis time
        )
        Time.now
