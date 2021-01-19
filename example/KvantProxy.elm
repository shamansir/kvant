port module KvantProxy exposing (..)


import Array exposing (Array)
import Browser
import Random
import Html exposing (div)


type alias TilesCount = Int


type alias AdjacencyRules = Array ()


type alias OutputSize = (Int, Int)


type alias Output = List (List Int)


type Msg
    = Calculate TilesCount AdjacencyRules OutputSize
    | SendOutput Output


type alias Model = ()


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    ( ()
    , case msg of
        Calculate count _ ( sizeX, sizeY ) ->
            Random.list sizeY (Random.list sizeX <| Random.int 0 <| count - 1)
                |> Random.generate SendOutput
        SendOutput output ->
            sendOutput output
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    triggerCalculation <|
        \{ count, rules, size } ->
            Calculate count rules size


main : Program {} Model Msg
main =
    Browser.element
        { init = always ( (), Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = always <| div [] []
        }


port sendOutput : Output -> Cmd msg


port triggerCalculation :
    (
        { count : TilesCount
        , rules : AdjacencyRules
        , size : OutputSize
        } -> msg
    ) -> Sub msg
