port module Worker exposing (..)

import Random
import Array exposing (Array)

import Kvant.Vec2 exposing (Vec2)
import Kvant.Core exposing (Wfc, TracingWfc)
import Kvant.Solver.History exposing (..)


type alias Source = Array (Array Int)

type alias StepResult = ( Int, Array (Array (Array Int)) )
type alias RunResult = Array (Array Int)


type alias Options = -- TODO
    {

    }


type Model
    = Empty
    | Running Random.Seed ( Wfc Vec2 Source Int )
    | Tracing Random.Seed ( TracingWfc Vec2 Int ) ( History Vec2 )


type Msg
    = Run Options Source
    | RunWith Random.Seed Options Source
    | Trace Options Source
    | TraceWith Random.Seed Options Source
    | Step
    | StepBack Random.Seed
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ run <| \{options, source} -> Run options source
        , step <| always Step
        ]


main : Program () Model Msg
main =
    Platform.worker
        { init = always ( Empty, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port run : ({ options : Options, source : Source } -> msg) -> Sub msg

port trace : ({ options : Options, source : Source } -> msg) -> Sub msg

port step : (() -> msg) -> Sub msg

port onResult : RunResult -> Cmd msg

port onStep : StepResult -> Cmd msg

-- TODO: getTiles:

-- TODO: getNeighbours

-- TODO: onStatus
