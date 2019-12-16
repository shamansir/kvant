module Main exposing (..)


import Browser

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Core exposing (WFC, TextWFC)
import WFC.Core as WFC
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (TextOptions)


type alias Model =
    { source: String
    , result: Maybe String
    }


type Msg
    = NoOp
    | Calculate WFC.Instance


init : Model
init =
    Model
    (
        "0000" ++
        "0111" ++
        "0121" ++
        "0111"
    )
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
                        WFC.Text wfc -> Just (wfc |> WFC.run model.source)
                }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div
        [ ]
        [ div [] [ text model.source ]
        , div [] [ text (model.result |> Maybe.withDefault "<NO RESULT>") ]
        ]


options : WFC.TextOptions
options =
    { approach = Overlapping
    , tileSize = ( 2, 2 )
    , inputSize = ( 4, 4 )
    , outputSize = ( 10, 10 )
    }


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

