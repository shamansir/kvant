port module Worker exposing (..)

import Random
import Task
import Time
import Array exposing (Array)
import Json.Decode as D
import Json.Encode as E

import Kvant.Vec2 exposing (Vec2)
import Kvant.Core exposing (Wfc, TracingWfc)
import Kvant.Core as Wfc exposing (make, makeAdvancing, noConvert)
import Kvant.Solver as Solver exposing (..)
import Kvant.Solver.Flat as FlatSolver exposing (init)
import Kvant.Solver.History exposing (..)
import Kvant.Solver.Options as Solver exposing (..)
import Kvant.Solver.Options as Options exposing (decode)
import Kvant.Plane exposing (N(..), Plane)
import Kvant.Plane.Flat as Plane exposing (..)


type alias Source = Array (Array Int)

type alias StepResult = Array (Array (Array Int))
type alias RunResult = StepResult


{- type Proceed
    = Manually
    | AutoUntil Int -}


type Model
    = Empty
    | Solution StepResult
    | Tracing ( Wfc.Wfc Vec2 (Plane Vec2 (List Int)) Int ) ( Solver.Step Vec2 ) -- ( History Vec2 )


type Msg
    = Run (Solver.Options Vec2) Source
    | RunWith (Solver.Options Vec2) Source Random.Seed
    | Trace (Solver.Options Vec2) Source
    | TraceWith (Solver.Options Vec2) Source Random.Seed
    | Step
    | StepBack
    | StepBackWith Random.Seed
    | Stop


defaultOptions : Solver.Options Vec2
defaultOptions =
    { approach =
        Overlapping
            { patternSize = N (2, 2)
            , inputBoundary = Bounded
            , symmetry = NoSymmetry
            }
    , outputBoundary = Bounded
    , outputSize = ( 10, 10 )
    }


-- TODO: remove
fromPlane : Plane Vec2 (List Int) -> Array (Array (Array Int))
fromPlane =
    Plane.unpack
        >> List.map (List.map <| Array.fromList << Maybe.withDefault [])
        >> List.map Array.fromList
        >> Array.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run options source ->
            ( model
            , makeSeedAnd <| RunWith options source
            )

        RunWith options source seed ->
            let
                solution =
                    Wfc.make Wfc.noConvert
                            (FlatSolver.init defaultOptions <| Plane.fromArrayGrid source)
                        |> Wfc.run seed
                        |> fromPlane
            in
                ( Solution solution
                , onResult solution
                )

        Trace options source ->
            ( model
            , makeSeedAnd <| TraceWith options source
            )

        TraceWith options source seed ->
            let
                tracingWfc = Wfc.makeAdvancing Wfc.noConvert
                    (FlatSolver.init defaultOptions <| Plane.fromArrayGrid source)
                ( nextStep, traceResult ) =
                    tracingWfc
                        |> Wfc.firstStep seed
                        --|> Tuple.mapSecond fromPlane
            in
                ( Tracing tracingWfc nextStep
                , onStep <| fromPlane traceResult
                )

        Step ->
            case model of
                Tracing tracingWfc prevStep ->
                    let
                        (nextStep, traceResult )
                            = tracingWfc
                                |> Wfc.step prevStep
                    in
                        ( Tracing tracingWfc nextStep
                        , onStep <| fromPlane traceResult
                        )
                    -- history |>
                    --             H.push ( lastStep, TracingStep lastTracingStep )
                _ -> ( model, Cmd.none )

        StepBack ->
            ( model
            , makeSeedAnd StepBackWith
            )

        StepBackWith newSeed ->
            {-
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
            -}
            ( model
            , Cmd.none
            )

        Stop ->
            ( Empty
            , Cmd.none
            )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ run <| \{options, source} ->
            case options |> D.decodeValue Solver.decode of
                Ok decodedOptions -> Run decodedOptions source
                Err _ -> Run defaultOptions source

        , trace <| \{options, source} ->
            case options |> D.decodeValue Solver.decode of
                Ok decodedOptions -> Trace decodedOptions source
                Err _ -> Trace defaultOptions source
        , step <| always Step
        , back <| always StepBack
        , stop <| always Stop
        ]


main : Program () Model Msg
main =
    Platform.worker
        { init = always ( Empty, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


makeSeedAnd : (Random.Seed -> msg) -> Cmd msg
makeSeedAnd makeMsg =
    Task.perform
        (\time ->
            makeMsg <| Random.initialSeed <| Time.posixToMillis time
        )
        Time.now


port run : ({ options : E.Value, source : Source } -> msg) -> Sub msg

port stop : (() -> msg) -> Sub msg

port trace : ({ options : E.Value, source : Source } -> msg) -> Sub msg

port step : (() -> msg) -> Sub msg

port back : (() -> msg) -> Sub msg

port onResult : RunResult -> Cmd msg

port onStep : StepResult -> Cmd msg

-- TODO: getTiles:

-- TODO: getNeighbours

-- TODO: onStatus
