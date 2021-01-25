module Example.Advance exposing (..)


import Kvant.Solver as Solver exposing (..)
import Kvant.Solver.History as H exposing (History)
import Kvant.Plane exposing (..)


type AdvanceMode
    = StepByStep
    | AtOnce


type Status fmt a
    = None
    | Preparation
    | Solving ( fmt, Plane a ) History
    | Solved ( fmt, Plane a)



type TracingStep = TracingStep Solver.Step
type alias History = H.History (Solver.Step, TracingStep)


doingSomething : Status fmt a -> Bool
doingSomething status =
    case status of
        None -> False
        _ -> True


isSolving : Status fmt a -> Bool
isSolving status =
    case status of
        None -> False
        Preparation -> False
        Solved _ -> False
        Solving _ _ -> True


-- getExampleModel : Example -> ExampleModel v fmt a
-- getExampleModel example =
--     case example of
--        TextExample model -> model


getCurrentPlane : Status fmt a -> Maybe ( fmt, Plane a )
getCurrentPlane status =
    case status of
        Solving plane _ -> Just plane
        Solved plane -> Just plane
        _ -> Nothing


getHistory : Status fmt a -> Maybe History
getHistory status =
    case status of
        Solving _ history -> Just history
        _ -> Nothing


unpackTracingStep : TracingStep -> Step
unpackTracingStep (TracingStep step) = step
