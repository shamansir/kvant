module Render.Grid exposing (..)


import Array

import Html exposing (..)
import Html.Attributes exposing (..)


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


{-
View any amount of elements in a grid, and so tries to fit them in a square
by calculating the size of a side which fits them best, and it is not a problem
if such grid is not completely filled with elements,
meaning there could exist empty spots in the end.

Can be used to display a grid with `Matches`.

Along with element, receives a `scale` which represents how elements in the grid
should be scaled to fit the grid. Scale `1.0` means that there's only one element in
the list from the argument and so it can take the whole 1x1 place.
-}
asGrid : a -> (Float -> a -> Html msg) -> List a -> Html msg
asGrid default viewElem  src =
    let
        findSide : Int -> Int
        findSide forN =
            let
                root = sqrt <| toFloat forN
            in
                if root <= 0 then 1
                else if (root - (toFloat <| ceiling root)) > 0 then
                    Basics.max 1 <| ceiling root + 1
                else Basics.max 1 <| ceiling root
        side = findSide <| List.length src
        scale = 1 / toFloat (side * side) -- 1 / toFloat side
        gridArr = src |> Array.fromList
        gridList =
            List.range 0 (side - 1)
                |> List.map (\x ->
                    List.range 0 (side - 1)
                    |> List.map (\y ->
                        gridArr
                            |> Array.get (x * side + y)
                            |> Maybe.withDefault default
                    )
                )
    in
        grid (viewElem scale) gridList
