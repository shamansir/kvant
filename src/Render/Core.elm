module Render.Core exposing (..)


import Array

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Occurrence as WFC exposing (..)
import WFC.Solver.History exposing (History)
import WFC.Solver.History as History



{-
type alias Spec v fmt a =
    { source : v -> fmt -> Html Never
    , tracing : TracingPlane v a -> Html Never
    , tracingTiny : TracingPlane v a -> Html Never
    , subPlanes : Plane v a -> Html Never
    , periodicSubPlanes : Plane v a -> Html Never
    , allViews : Plane v a -> Html Never
    , patterns : Plane.SearchMethod -> N v -> Plane v a -> Html Never
    , allSubPlanes : Plane.SearchMethod -> N v -> Plane v a -> Html Never
    , rotationsAndFlips : Plane v a -> Html Never
    , materialized : Plane v a -> Html Never
    , step : WFC.Step v -> Html Never
    , history : History (Step v) -> Html Never
    }
-}


withIndex : Int -> Html msg -> Html msg
withIndex index subView =
    div []
        [ text <| String.fromInt index ++ "."
        , subView
        ]


occursText : WFC.Occurrence -> String
occursText occured =
    case occured of
        Unknown -> "occurs unknown amount of times"
        Times howMuch -> "occurs " ++ String.fromInt howMuch ++ " times"


listBy : (a -> Html msg) -> List a -> Html msg
listBy viewItem items =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
        <| List.map viewItem items


history : (a -> Html msg) -> History a -> Html msg
history viewItem theHistory =
    div
        [ style "position" "absolute"
        , style "right" "0"
        , style "margin-top" "-400px"
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        <| (History.toList theHistory
            |> List.map viewItem)
