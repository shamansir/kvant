port module Worker exposing (..)

import Random
import Task
import Time
import Array exposing (Array)
import Json.Decode as D
import Json.Encode as E

import Kvant.Vec2 as Vec2
import Kvant.Core exposing (Wfc)
import Kvant.Core as Wfc
import Kvant.Solver as Solver
import Kvant.Solver.Options as Solver
import Kvant.Solver.Options exposing (Approach(..))
import Kvant.Plane exposing (..)


type alias Source = Array (Array Int)

type alias StepResult = Array (Array (Array Int))
type alias RunResult = StepResult


{- type Proceed
    = Manually
    | AutoUntil Int -}


type Model
    = Empty
    | Solution StepResult
    | Tracing Wfc Solver.Step -- ( History Vec2 )


type Msg
    = Run Solver.Options Source
    | RunWith Solver.Options Source Random.Seed
    | Trace Solver.Options Source
    | TraceWith Solver.Options Source Random.Seed
    | Step
    | StepBack
    | StepBackWith Random.Seed
    | Stop


defaultOptions : Solver.Options
defaultOptions =
    { approach =
        Overlapping
            { patternSize = (2, 2)
            , inputBoundary = Bounded
            , symmetry = NoSymmetry
            }
    , outputBoundary = Bounded
    , outputSize = ( 10, 10 )
    }


-- TODO: remove
fromPlane : Plane (List Int) -> Array (Array (Array Int))
fromPlane =
    toArray2d
        >> Array.map (Array.map (Maybe.withDefault [] >> Array.fromList))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Run options source ->
            ( model
            , makeSeedAnd <| RunWith options source
            )

        RunWith options source seed ->
            let
                sourcePlane =
                    source
                        |> fromArray2d
                            (Vec2.loadSize source |> Maybe.withDefault (0, 0))
                solution =
                    Wfc.make options sourcePlane
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
                sourcePlane =
                    source
                        |> fromArray2d
                            (Vec2.loadSize source |> Maybe.withDefault (0, 0))
                tracingWfc =
                    Wfc.makeAdvancing options sourcePlane
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
