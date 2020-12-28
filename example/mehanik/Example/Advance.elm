module Example.Advance exposing (..)


import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Solver as Solver exposing (..)
import Kvant.Solver.History as H exposing (History)


type AdvanceMode
    = StepByStep
    | AtOnce


type Status v fmt a
    = None
    | Preparation
    | Solving ( fmt, TracingPlane v a ) (History v)
    | Solved ( fmt, TracingPlane v a )



type TracingStep v = TracingStep (Solver.Step v)
type alias History v = H.History (Solver.Step v, TracingStep v)


doingSomething : Status v fmt a -> Bool
doingSomething status =
    case status of
        None -> False
        _ -> True


isSolving : Status v fmt a -> Bool
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
