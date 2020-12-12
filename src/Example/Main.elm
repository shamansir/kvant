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
import Kvant.Plane.Impl.Image exposing (Pixels)
import Kvant.Solver exposing (Step)
import Kvant.Solver as Solver exposing (Options)
import Kvant.Solver.History as H exposing (History)


type TracingStep v = TracingStep (Solver.Step v)
type alias History v = H.History (Solver.Step v, TracingStep v)


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
    { source : fmt -> Html msg
    , tracing : TracingPlane v a -> Html msg
    , tracingTiny : TracingPlane v a -> Html msg
    , subPlanes : Plane v a -> Html msg
    , periodicSubPlanes : Plane v a -> Html msg
    , allViews : Plane v a -> Html msg
    , rotationsAndFlips : Plane v a -> Html msg
    , materialized : Plane v a -> Html msg
    , patterns : Boundary -> N v -> Plane v a -> Html msg
    , allSubPlanes : Boundary -> N v -> Plane v a -> Html msg
    , step : Step v -> Html msg
    , history : H.History (Step v) -> Html msg
    }


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


type AdvanceMode
    = StepByStep
    | AtOnce


type alias TextBlock = Block Vec2 String Char


type alias ExampleModel v fmt a =
    { source : fmt
    , sourcePlane : Plane v a
    , options : Solver.Options v a
    , expands : List BlockState
    , wfc : ( Wfc v fmt a, TracingWfc v a )
    , makeWfc : AdvanceMode -> ( Wfc v fmt a, TracingWfc v a )
    , status : Status v fmt a
    }


type alias TextExample = ExampleModel Vec2 BoundedString Char
type alias ImageExample = ExampleModel Vec2 Image Color
type alias PixelsExample = ExampleModel Vec2 Pixels Color


make
    :  (AdvanceMode -> ( Wfc v fmt a, TracingWfc v a ) )
    -> Solver.Options v a
    -> fmt
    -> Plane v a
    -> ExampleModel v fmt a
make makeWfc options src sourcePlane =
    { source = src
    , sourcePlane = sourcePlane
    , options = options
    , expands = []
    , wfc = makeWfc AtOnce
    , makeWfc = makeWfc
    , status = None
    }
    |> initExpands


blocks : ExampleModel v fmt a -> List (Block v fmt a)
blocks e =
    [ Source e.source
    , RunOnce e.options.outputSize e.status
    , Tracing e.status
    , RotationsAndFlips e.sourcePlane
    , SubPlanes e.sourcePlane
    , PeriodicSubPlanes e.sourcePlane
    , AllViews e.sourcePlane
    ]
    ++
    (case e.options.approach of
        Solver.Overlapping { patternSize, searchBoundary } ->
            [ Patterns
                    e.sourcePlane
                    searchBoundary
                    patternSize
            , AllSubPlanes
                    e.sourcePlane
                    searchBoundary
                    patternSize
            ]
        Solver.Tiled -> [] -- FIXME: implement
    )


view
    :  (ExampleMsg -> msg)
    -> Renderer v fmt a ExampleMsg
    -> ExampleModel v fmt a
    -> Html msg
view toOtherMsg renderer exampleModel =
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


update
    :  ExampleMsg
    -> ExampleModel v fmt a
    -> ( ExampleModel v fmt a, Cmd ExampleMsg )
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

        SwitchBlock index ->
            (
                { model
                | expands =
                    model.expands |> switchBlock index
                }
            , Cmd.none )


initExpands : ExampleModel v fmt a -> ExampleModel v fmt a
initExpands exampleModel =
    { exampleModel
    | expands =
        blocks exampleModel
            |> List.map
                (\block ->
                    case block of
                        Source _ -> Expanded
                        RunOnce _ _-> Expanded
                        Tracing _ -> Expanded
                        _ -> Collapsed
                )
    }


blockTitle : Block v fmt a -> String
blockTitle block =
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


viewBlock : Renderer v fmt a ExampleMsg -> Block v fmt a -> Html ExampleMsg
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