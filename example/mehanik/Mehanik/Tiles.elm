module Mehanik.Tiles exposing (..)

-- import


import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Kvant.Plane as Plane exposing (Plane)
import Kvant.Rotation as Rotation exposing (Rotation)
import Kvant.Tiles exposing (TileKey, noTile, TileGrid)
import Kvant.Vec2 as Vec2

import Mehanik.Grid exposing (grid, grid2)
import Mehanik.Generic exposing (viewList)


type alias TileUrl = String


-- FIXME: same as Generic.viewPatterns
viewTiles toTileUrl toMsg mapping format group =
    viewList
        (\tileKey ->
            toMsg
                <| Maybe.withDefault -1
                <| Dict.get tileKey
                <| Tuple.second
                <| mapping
        )
        (tile1 (toTileUrl format group)
                        << Tuple.mapSecond Rotation.fromId)


renderInput : (( TileKey, Rotation ) -> String) -> TileGrid -> Html msg
renderInput toPath =
    grid2 (tile1 toPath)


renderPlane : (( TileKey, Rotation ) -> String) -> Plane ( TileKey, Rotation ) -> Html msg
renderPlane toPath =
    Plane.toList2d
        >> List.map (List.map <| Maybe.withDefault (noTile, Rotation.Original))
        >> grid (tile1 toPath)



tile : String -> Html msg
tile path =
    img
        [ src path
        , width 50
        , height 50
        ]
        [ ]


tile1 : (( TileKey, Rotation ) -> String) -> ( TileKey, Rotation ) -> Html msg
tile1 toPath ( key, rotation ) =
    div
        [ style "transform"
            <| "rotate(" ++ String.fromFloat (Rotation.toAngle rotation) ++ "deg)"
        , style "max-height" "50px"
        , style "max-width" "50px"
        ]
        [ {- span [ style "position" "absolute" ] [ text <| Rotation.toString rotation ]
        , -} tile <| toPath ( key, rotation )
        ]


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









type alias TilesPlane = Plane (TileKey, Rotation)


make : TileGrid -> TilesPlane
make tileGrid =
    Plane.fromArray2d
        (Vec2.loadSize tileGrid |> Maybe.withDefault (0, 0))
        tileGrid


toGrid : TilesPlane -> List (List (TileKey, Rotation))
toGrid plane =
    Plane.toList2d plane
        |> List.map (List.map <| Maybe.withDefault ( noTile, Rotation.Original ))


merge : List (TileKey, Rotation) -> ( (TileKey, Rotation), Int )
merge tiles =
    ( List.head tiles |> Maybe.withDefault ( noTile, Rotation.Original )
    , List.length tiles
    )
