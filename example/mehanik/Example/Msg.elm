module Example.Msg exposing (..)

import Random

type Msg
    = TriggerRunning
    | TriggerTracing
    | TriggerPreviousStep
    | TriggerFastForward
    | Run Random.Seed
    | Trace Random.Seed
    | NextStep
    | PreviousStep Random.Seed
    | Stop
