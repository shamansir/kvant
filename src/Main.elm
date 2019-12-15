module Main exposing (..)


import Browser

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Core exposing (WFC(..))
import WFC.Core as WFC exposing (string)


type alias Model =
    { source: String
    , result: Maybe String
    }


type Msg
    = NoOp
    | Calculate (WFC String)

-- type alias Document msg =
--   { title : String
--   , body : List (Html msg)
--   }


init : Model
init = Model "123456789" Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )
        Calculate (WFC wfc) ->
            (
                { model
                | result = Just <| wfc model.source
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


    -- { init : flags -> Url -> Key -> ( model, Cmd msg )
    -- , view : model -> Document msg
    -- , update : msg -> model -> ( model, Cmd msg )
    -- , subscriptions : model -> Sub msg
    -- , onUrlRequest : UrlRequest -> msg
    -- , onUrlChange : Url -> msg
    -- }

main : Program {} Model Msg
main =
    Browser.application
        { init = \_ _ _ -> init |> update (Calculate WFC.string)
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "WFC", body = [ view model ] }
        }

