module Example.Msg exposing (..)

import Random

type Msg
    = TriggerRunning
    | TriggerTracing
    | TriggerPreviousStep
    | Run Random.Seed
    | Trace Random.Seed
    | NextStep
    | PreviousStep Random.Seed
    | Stop
    | SwitchBlock Int
