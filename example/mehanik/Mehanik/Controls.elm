module Mehanik.Controls exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


fancyButton : Bool -> String -> msg -> Html msg
fancyButton isEnabled label msg =
    button
        [ onClick msg
        , style "border" "none"
        , style "background" "none"
        , style "cursor" "pointer"
        , style "outline" "none"
        , style "opacity" <| if isEnabled then "1.0" else "0.5"
        , disabled <| not isEnabled
        ]
        [ Html.text label
        ]


checkbox : Bool -> Bool -> String -> msg -> Html msg
checkbox isChecked isEnabled label msg =
    span
        []
        [input
            [ type_ "radio"
            , onClick msg
            , checked isChecked
            , disabled <| not isEnabled
            , style "border"
                <| if isChecked
                    then "1px solid aqua"
                    else "1px solid black"
            , style "padding" "5px"
            , style "margin" "5px"
            , style "background" "none"
            , style "cursor" "pointer"
            , style "outline" "none"
            , style "display" "inline-block"
            , value label
            ] []
        , Html.text label
        ]


controlPanel : String -> List (Html msg) -> Html msg
controlPanel title items =
    div [ style "display" "flex"
        , style "padding" "5px"
        , style "margin" "5px"
        , style "border" "1px solid black"
        , style "border-radius" "3px"
        , style "width" "fit-content"
        ]
        (
            span
                [ style "font-size" "10px"
                , style "color" "white"
                , style "background" "gray"
                , style "padding" "3px"
                , style "border-radius" "3px"
                , style "max-height" "1em"
                ]
                [ Html.text title ]
            :: items
        )
