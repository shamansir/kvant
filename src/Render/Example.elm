module Render.Example exposing (..)


import Random
import Task
import Time

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Color exposing (Color)
import Image exposing (Image)

import WFC.Vec2 exposing (..)
import WFC.Core as WFC exposing (..)
import WFC.Core as Core exposing (firstStep)
import WFC.Plane exposing (Plane, N)
import WFC.Plane.Flat exposing (SearchMethod)
import WFC.Plane.Impl.Tracing exposing (TracingPlane)
import WFC.Solver exposing (Step)
import WFC.Solver as WFC exposing (Options)
import WFC.Solver.History as H exposing (History)


type TracingStep v = TracingStep (WFC.Step v)
type alias History v = H.History (WFC.Step v, TracingStep v)


type Status v fmt a
    = None
    | Preparation
    | Solving ( fmt, TracingPlane v a ) (History v)
    | Solved ( fmt, TracingPlane v a )


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


type alias Renderer v fmt a msg =
    { source : v -> fmt -> Html msg
    , tracing : TracingPlane v a -> Html msg
    , tracingTiny : TracingPlane v a -> Html msg
    , subPlanes : Plane v a -> Html msg
    , periodicSubPlanes : Plane v a -> Html msg
    , allViews : Plane v a -> Html msg
    , rotationsAndFlips : Plane v a -> Html msg
    , materialized : Plane v a -> Html msg
    , patterns : SearchMethod -> N v -> Plane v a -> Html msg
    , allSubPlanes : SearchMethod -> N v -> Plane v a -> Html msg
    , step : Step v -> Html msg
    , history : History (Step v) -> Html msg
    }


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
    , wfc : ( WFC v fmt a, TracingWFC v a )
    , status : Status v fmt a
    }


type Example
    = TextExample (ExampleModel Vec2 String Char)
    | ImageExample (ExampleModel Vec2 Image Color)


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


viewBlock : Renderer v fmt a ExampleMsg -> Block v fmt a -> Html ExampleMsg
viewBlock render block =
    case block of

        Source bounds source ->
            source |> render.source bounds -- |> Html.map (always NoOp)
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
                    |> Maybe.map (render.source bounds)
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


viewBlocksOf
    :  (ExampleMsg -> msg)
    -> Renderer v fmt a ExampleMsg
    -> ExampleModel v fmt a
    -> Html msg
viewBlocksOf toOtherMsg renderer exampleModel =
    let
        viewBlockItem blockIndex ( isExpanded, block ) =
            div []
                [
                    span
                        [ style "cursor" "pointer"
                        , onClick
                            <| toOtherMsg
                            <| SwitchBlock blockIndex
                        ]
                        [ Html.text <| blockTitle block
                        ]
                , case isExpanded of
                    Expanded ->
                        viewBlock renderer block
                            |> Html.map toOtherMsg
                    Collapsed -> Html.text "..."
                ]
    in
        div []
            (blocks exampleModel
                |> List.map2 Tuple.pair exampleModel.expands
                |> List.indexedMap viewBlockItem
                |> List.intersperse (hr [] [])
            )


viewExample
    :  (ExampleMsg -> msg)
    -> Renderer v fmt a ExampleMsg
    -> Example
    -> Html msg
viewExample toOtherMsg renderer example =
    case example of
        TextExample exampleModel  -> viewBlocksOf toOtherMsg renderer exampleModel
        ImageExample exampleModel -> viewBlocksOf toOtherMsg renderer exampleModel


update
    :  ExampleId
    -> ExampleMsg
    -> ExampleModel v fmt a
    -> ( ExampleModel v fmt a, Cmd ExampleMsg )
update id msg model =
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
                        model.wfc |> Tuple.first |> Core.firstStep seed
                    (lastTracingStep, tracingResult) =
                        model.wfc |> Tuple.second |> Core.firstStep seed
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


unpackTracingStep : TracingStep v -> Step v
unpackTracingStep (TracingStep step) = step


makeSeedAnd : (Random.Seed -> msg) -> Cmd msg
makeSeedAnd makeMsg =
    Task.perform
        (\time ->
            makeMsg <| Random.initialSeed <| Time.posixToMillis time
        )
        Time.now
