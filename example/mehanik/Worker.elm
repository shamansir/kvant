port module Worker exposing (..)

import Random

import Kvant.Vec2 exposing (Vec2)
import Kvant.Core exposing (TracingWfc)


type alias Model = Maybe ( Random.Seed, TracingWfc Vec2 Int )


type Msg
    = Run
    | Step
    | StepBack
    | Stop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


main : Program () Model Msg
main =
    Platform.worker
        { init = always ( Nothing, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


port run : (() -> msg) -> Sub msg

port step : (() -> msg) -> Sub msg

port onResult : () -> Cmd msg

port onStep : () -> Cmd msg

-- TODO: getTiles:

-- TODO: getNeighbours
