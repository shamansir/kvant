module Example.Instance.Tiles.Render exposing (..)


import Html exposing (..)
import Html.Attributes exposing (..)


import Kvant.Vec2 exposing (..)
import Kvant.Plane.Flat as Plane exposing (unpack)

import Example.Render as Render exposing (..)

import Example.Instance.Tiles exposing (TilesRegistry)
import Example.Instance.Tiles.Plane exposing (merge, TileGrid, TileId)


make : Renderer Vec2 TileGrid TileId (Html msg)
make =
    { source = grid tile
    , plane =
        Plane.unpack
            >> List.map (List.map <| Maybe.withDefault noTile)
            >> grid tile
    , tracingPlane = always <| div [] []
    , tracingCell = always <| div [] []
    }


tile : TileId -> Html msg
tile _ =
    div [] []


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
