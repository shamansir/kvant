module Mehanik.Chars exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)
import Array exposing (Array)

import Kvant.Vec2 as V2 exposing (Vec2)
import Kvant.Plane as Plane exposing (Plane(..))



type alias TextPlane = Plane Char


type alias BoundedString = (Vec2, String)


make : Vec2 -> String -> TextPlane
make ( width, height ) src =
    let
        charList =
            String.toList src
                |> List.indexedMap
                    (\index c ->
                        (
                            ( index // width
                            , index |> modBy width
                            )
                        , c
                        )
                    )

    in
        Plane.fromList
            ( width, height )
            charList


toString : TextPlane -> String
toString =
    Plane.toList2d
        >> List.concat
        >> List.filterMap identity
        >> String.fromList


toBoundedString : TextPlane -> BoundedString
toBoundedString (Plane size _ as plane) =
    ( size
    , plane |> toString
    )


toBoundedStringFromGrid : Array (Array Char) -> BoundedString
toBoundedStringFromGrid theGrid =
    ( V2.loadSize theGrid |> Maybe.withDefault (0, 0)
    , theGrid
        |> Array.map Array.toList
        |> Array.toList
        |> List.concat
        |> String.fromList
    )


boundedStringToGrid : BoundedString -> List (List Char)
boundedStringToGrid ((width, height), string) =
    let
        splitBy : Int -> String -> List String
        splitBy w src =
            let
                next = src |> String.left w
                left = src |> String.dropLeft w
            in
                if String.length left > 0 then
                    next :: splitBy w left
                else
                    [ next ]
    in
        string
            |> splitBy width
            |> List.map (String.toList)


merge : List Char -> Char
merge chars =
    let
        optionsCount = List.length chars
        maybeFirst = List.head chars
    in
        if optionsCount <= 1 then
            maybeFirst |> Maybe.withDefault 'x'
        else if optionsCount <= 20 then
            Char.fromCode <| 9312 + (optionsCount - 1)
        else Char.fromCode <| 9398 + optionsCount








{- spec : HtmlSpec Vec2 Char msg
spec =
    { default = '?'
    , contradiction = '∅'
    , a = char
    , v = Render.coord
    , merge = merge
    , scaled = scaled
    , vToString = Render.coordText
    } -}


renderInput : BoundedString -> Html msg
renderInput =
    boundedStringToGrid >> grid char


renderPlane : Plane Char -> Html msg
renderPlane =
    Plane.toList2d
        >> List.map (List.map <| Maybe.withDefault '?')
        >> grid char


char : Char -> Html msg
char c =
    span
        [ style "display" "inline-block"
        , style "width" "9px"
        , style "background-color" <| symbolBg c
        , style "padding" "2px 8px"
        , style "color" <|
            if symbolBg c == "black" then
                "rgba(255,255,255,0.3)"
            else
                "rgba(0,0,0,0.3)"
        ]
        [ text <| String.fromChar c ]


symbolBg : Char -> String
symbolBg symbol =
    case symbol of
        '0' -> "white"
        '1' -> "black"
        '2' -> "red"
        '3' -> "aqua"
        '4' -> "blue"
        '5' -> "green"
        '6' -> "teal"
        '7' -> "maroon"
        '8' -> "#85C1E9"
        '9' -> "#5D6D7E"
        'A' -> "#5DADE2"
        'B' -> "#F9E79F"
        'C' -> "#F4D03F"
        'D' -> "#E74C3C"
        'E' -> "#BFC9CA"
        'F' -> "#2E86C1"
        'x' -> "salmon"
        _ -> "lightgray"


scaled : Float -> Char -> Html msg
scaled scale c =
    span
        [ style "transform" ("scale(" ++ String.fromFloat scale ++ ")")
        , style "width" "10px"
        , style "height" "10px" ]
        [ text <| String.fromChar c ]


grid : (a -> Html msg) -> List (List a) -> Html msg
grid viewElem rows =
    rows
        |> List.map
            (\row ->
                div [ style "display" "flex", style "flex-direction" "row" ]
                    <| List.map viewElem row
            )
        |> div [ style "display" "flex", style "flex-direction" "column" ]


gridV : (v -> a -> Html msg) -> List (List (v, a)) -> Html msg
gridV viewElem = grid (\(v, a) -> viewElem v a)  -- a.k.a. `uncurry`


grid1 : (Array a -> Html msg) -> Array (Array (Array a)) -> Html msg
grid1 viewElem =
    Array.map Array.toList >> Array.toList >> grid viewElem
