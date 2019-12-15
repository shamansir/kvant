module Main exposing (..)


import Browser

import Html exposing (..)


type Model = Model



type Msg = Msg

-- type alias Document msg =
--   { title : String
--   , body : List (Html msg)
--   }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ = ( Model, Cmd.none )


view : Model -> Html Msg
view _ = div [] [ text "WFC" ]


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
        { init = \_ _ _ -> ( Model, Cmd.none )
        , onUrlChange = always Msg
        , onUrlRequest = always Msg
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "WFC", body = [ view model ] }
        }

