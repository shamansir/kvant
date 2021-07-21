module Mehanik.RuleEditor exposing (..)

import Kvant.Rotation as Rotation exposing (Rotation(..))
import Kvant.Symmetry as Symmetry exposing (Symmetry(..))
import Kvant.Tiles exposing (Rule, TileInfo, TileKey)

import Html exposing (Html)
import Html.Events as Html


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
    Html.div
        []
        [ Html.div [] <| List.map (viewTileInfo viewTile) <| tiles
        , Html.div [] <| List.map (viewRule viewTile) <| rules
        ]


viewTileInfo : ((TileKey, Rotation) -> Html msg) -> TileInfo -> Html Msg
viewTileInfo viewTile { key, symmetry, weight } =
    Html.div
        []
        [ Html.span [] [ Html.text key ]
        , case symmetry of
            Just s ->
                Html.div
                    [ {- Html.onClick NoOp -} ]
                    [ Html.text
                        <| "Symmetry: ("
                            ++ Symmetry.symmetryToString s
                            ++ ")"
                    , viewSymmetry (\rot -> viewTile (key, rot)) key s
                        |> Html.map (always NoOp)
                    ]
            Nothing -> Html.span [] []

        ]


viewRule : ((TileKey, Rotation) -> Html msg) -> Rule -> Html Msg
viewRule viewTile { left, right } =
    Html.div
        []
        [ viewTile left |> Html.map (always NoOp)
        , viewTile right |> Html.map (always NoOp)
        ]


viewSymmetry : ( Rotation -> Html msg ) -> TileKey -> Symmetry -> Html msg
viewSymmetry viewRotation tile symmetry =
    Html.div
        []
        <| List.map
            (\rotation ->
                Html.div
                    []
                    [ Html.text <| Rotation.toString rotation
                    , viewRotation rotation
                    ]
            )
        <| Rotation.uniqueFor symmetry