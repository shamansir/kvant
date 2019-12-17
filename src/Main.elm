module Main exposing (..)


import Browser

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Core exposing (WFC, TextWFC)
import WFC.Core as WFC
import WFC.Plane exposing (TextPlane, makeTextPlane, unpack2)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (TextOptions)


type alias Model =
    { source: String
    , result: Maybe String
    }


type Msg
    = NoOp
    | Calculate WFC.Instance


options : WFC.TextOptions
options =
    { approach = Overlapping
    , tileSize = ( 2, 2 )
    , inputSize = ( 4, 4 )
    , outputSize = ( 10, 10 )
    }


init : Model
init =
    let
        srcText =
            (
                "0000" ++
                "0111" ++
                "0121" ++
                "0111"
            )
    in
        Model
        srcText
        Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Calculate wfcInstance ->
            (
                { model
                | result =
                    case wfcInstance of
                        WFC.Text wfc ->
                            Just ( wfc |> WFC.run model.source )
                }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ ]
        [ model.source
            |> displayTextInBounds options.inputSize
        , hr [] []
        , model.result
            |> Maybe.map (displayTextInBounds options.outputSize)
            |> Maybe.withDefault (div [] [])
        ]


main : Program {} Model Msg
main =
    Browser.application
        { init = \_ _ _ ->
                    init |> update
                        (Calculate <| WFC.Text <| WFC.text options)
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "WFC", body = [ view model ] }
        }


splitBy : Int -> String -> List String
splitBy width src =
    let
        next = src |> String.left width
        left = src |> String.dropLeft width
    in
        if String.length left > 0 then
            next :: splitBy width left
        else
            [ next ]


displayCharGrid : List (List Char) -> Html Msg
displayCharGrid grid =
    grid
        |> List.map
            (\row ->
                div [ style "flex" "row" ]
                    (List.map
                        (\c ->
                            span
                                []
                                [ text <| String.fromChar c ]
                        )
                    row)
            )
        |> div [ style "flex" "column" ]


displayTextInBounds : ( Int, Int ) -> String -> Html Msg
displayTextInBounds (width, height) string =
    string
        |> splitBy width
        |> List.map (String.toList)
        |> displayCharGrid


displayTextPlane : ( Int, Int ) -> TextPlane -> Html Msg
displayTextPlane size plane =
    unpack2 size plane
        |> List.map (List.map <| Maybe.withDefault '?')
        |> displayCharGrid
