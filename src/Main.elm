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
    { source: ( String, TextPlane )
    , result: Maybe ( String, TextPlane )
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
        -- plane is needed only for text, to display it nicely
        srcPlane = srcText |> makeTextPlane options.inputSize
    in
        Model
        ( srcText, srcPlane )
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
                            let
                                resultText = wfc |> WFC.run (Tuple.first model.source)
                                resultPlane = resultText |> makeTextPlane options.outputSize
                            in
                                Just ( resultText, resultPlane )
                }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ ]
        [ model.source
            |> Tuple.second
            |> displayTextPlane options.inputSize
        , hr [] []
        , model.result
            |> Maybe.map Tuple.second
            |> Maybe.map (displayTextPlane options.outputSize)
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


displayTextPlane : ( Int, Int ) -> TextPlane -> Html Msg
displayTextPlane size plane =
    unpack2 size plane
        |> List.map
            (\row ->
                div [ style "flex" "row" ]
                    (List.map
                        (\c ->
                            span
                                []
                                [ c |> Maybe.withDefault '?' |> String.fromChar |> text ]
                        )
                    row)
            )
        |> div [ style "flex" "column" ]
