module Example.Instance.Tiles.Render exposing (..)


import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)


import Kvant.Vec2 exposing (..)
import Kvant.Plane.Flat as Plane exposing (unpack)

import Example.Render as Render exposing (..)

import Example.Instance.Tiles.Plane exposing (..)


make : (TileKey -> String) -> Renderer Vec2 TileGrid TileKey (Html msg)
make toPath =
    { source =
        Array.toList >> List.map Array.toList >> grid (tile1 toPath)
    , plane =
        Plane.unpack
            >> List.map (List.map <| Maybe.withDefault noTile)
            >> grid (tile1 toPath)
    , tracingPlane = always <| div [] []
    , tracingCell = always <| div [] []
    }


tile : String -> Html msg
tile path =
    img
        [ src path
        , width 50
        , height 50
        ]
        [ ]


tile1 : (TileKey -> String) -> TileKey -> Html msg
tile1 toPath key =
    tile <| toPath key


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
