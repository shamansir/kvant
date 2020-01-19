module Render.Core exposing (..)


import Array

import Html exposing (..)
import Html.Attributes exposing (..)



viewWithIndex : Int -> Html msg -> Html msg
viewWithIndex index subView =
    div []
        [ text <| String.fromInt index ++ "."
        , subView
        ]


occursText : Occurrence -> String
occursText occured =
    case occured of
        Unknown -> "occurs unknown amount of times"
        Times howMuch -> "occurs " ++ String.fromInt howMuch ++ " times"


viewHistory : History (WFC.Step Vec2) -> Html msg
viewHistory history =
    div
        [ style "position" "absolute"
        , style "right" "0"
        , style "margin-top" "-400px"
        , style "display" "flex"
        , style "flex-direction" "column"
        ]
        <| (History.toList history
            |> List.map viewStepStatus)
