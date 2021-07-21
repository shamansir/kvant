module Mehanik.RuleEditor exposing (..)

import Kvant.Rotation as Rotation exposing (Rotation(..))
import Kvant.Symmetry as Symmetry exposing (Symmetry(..))
import Kvant.Tiles exposing (Rule, TileInfo, TileKey)

import Html exposing (Html)
import Html as H
import Html.Attributes as HA
import Html.Events as H


type Side
    = Left
    | Right


type Msg
    = NoOp
    | AddRule
    | ChangeWeight TileKey Float
    | ChangeSymmetry TileKey Symmetry
    | DeleteRule Int
    | ChangeRule Int Side ( TileKey, Rotation )
    | SelectTile ( TileKey, Rotation )


type alias Model =
    { tiles : List TileInfo
    , rules : List Rule
    , currentRule : Maybe ( Int, Side )
    }


load : List TileInfo -> List Rule -> Model
load tiles rules = { tiles = tiles, rules = rules, currentRule = Nothing }


get : Model -> List Rule
get { rules } = rules


update : Msg -> Model -> Model
update msg model = model


view : Model -> ((TileKey, Rotation) -> Html msg) -> Html Msg
view { tiles, rules, currentRule } viewTile =
    H.div
        [ HA.class "rule-editor" ]
        [ H.div [ HA.class "tiles-info" ] <| List.map (viewTileInfo viewTile) <| tiles
        , H.div [ HA.class "tiles-rules" ] <| List.map (viewRule viewTile) <| rules
        ]


viewTileInfo : ((TileKey, Rotation) -> Html msg) -> TileInfo -> Html Msg
viewTileInfo viewTile { key, symmetry, weight } =
    H.div
        [ HA.class "tile-info" ]
        [ H.span [] [ H.text key ]
        , case symmetry of
            Just s ->
                H.div
                    [ {- H.onClick NoOp -} ]
                    [ H.text
                        <| "Symmetry: ("
                            ++ Symmetry.symmetryToString s
                            ++ ")"
                    , viewSymmetry (\rot -> viewTile (key, rot)) key s
                        |> H.map (always NoOp)
                    , case weight of
                        Just w -> H.text <| String.fromFloat w
                        Nothing -> H.span [] []
                    ]
            Nothing -> H.span [] []
        ]


viewRule : ((TileKey, Rotation) -> Html msg) -> Rule -> Html Msg
viewRule viewTile { left, right } =
    H.div
        [ HA.class "tile-rule" ]
        [ viewTile left |> H.map (always NoOp)
        , viewTile right |> H.map (always NoOp)
        ]


viewSymmetry : ( Rotation -> Html msg ) -> TileKey -> Symmetry -> Html msg
viewSymmetry viewRotation tile symmetry =
    H.div
        [ HA.class "symmetries" ]
        <| List.map
            (\rotation ->
                H.div
                    [ HA.class "symmetry" ]
                    [ H.span [ HA.class "rotation" ]  [ H.text <| Rotation.toString rotation ]
                    , H.div [ HA.class "tile" ] [ viewRotation rotation ]
                    ]
            )
        <| Rotation.uniqueFor symmetry