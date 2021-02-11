module Example.Instance.Tiles.Render exposing (..)


import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)


import Kvant.Vec2 exposing (..)
import Kvant.Plane as Plane
import Kvant.Tiles exposing (TileKey, Rotation, noTile, TileGrid)

import Example.Render exposing (Renderer)


make : (( TileKey, Rotation ) -> String) -> Renderer TileGrid ( TileKey, Rotation ) (Html msg)
make toPath =
    ( Array.toList >> List.map Array.toList >> grid (tile1 toPath)
    , Plane.toList2d
        >> List.map (List.map <| Maybe.withDefault (noTile, 0))
        >> grid (tile1 toPath)
    )


tile : String -> Html msg
tile path =
    img
        [ src path
        , width 50
        , height 50
        ]
        [ ]


rotationToAngle : Rotation -> Float
rotationToAngle rotation =
    case rotation of
        0 -> 0
        1 -> 90
        2 -> 180
        3 -> 270
        _ -> 360


tile1 : (( TileKey, Rotation ) -> String) -> ( TileKey, Rotation ) -> Html msg
tile1 toPath ( key, rotation ) =
    div
        [ style "transform"
            <| "rotate(" ++ String.fromFloat (rotationToAngle rotation) ++ "deg)"
        , style "max-height" "50px"
        , style "max-width" "50px"
        ]
        [ tile <| toPath ( key, rotation ) ]


tileAndCount : (( TileKey, Rotation ) -> String) -> ( ( TileKey, Rotation ), Int ) -> Html msg
tileAndCount toPath ( key, count ) =
    if count <= 1 then
        tile1 toPath key
    else
        div
            [ style "position" "relative" ]
            [ span
                [ style "position" "absolute" ]
                [ text <| String.fromInt count
                ]
            , tile1 toPath key
            ]


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
grid1 viewElem =
    Array.map Array.toList >> Array.toList >> grid viewElem
