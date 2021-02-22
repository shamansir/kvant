module Mehanik.Grid exposing (..)

import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Grid a = Array (Array (Array a))


mapGrid : (a -> b) -> Grid a -> Grid b
mapGrid = adaptGrid << Array.map


adaptGrid : (Array a -> b) -> Grid a -> Array (Array b)
adaptGrid f = Array.map <| Array.map f


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


-- gridXY : (Int, Int) -> ((Int, Int) -> Html msg) -> Html msg
-- gridXY size viewElem = grid (\(v, a) -> viewElem v a)  -- a.k.a. `uncurry`


grid1 : (Array a -> Html msg) -> Array (Array (Array a)) -> Html msg
grid1 =
    grid2


grid2 : (a -> Html msg) -> Array (Array a) -> Html msg
grid2 viewElem =
    Array.map Array.toList >> Array.toList >> grid viewElem
