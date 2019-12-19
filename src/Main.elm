module Main exposing (..)


import Browser

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Core exposing (WFC, TextWFC)
import WFC.Core as WFC
import WFC.Plane exposing (..)
import WFC.Plane as Plane exposing (sub ,subAt)
import WFC.Solver exposing (Approach(..), fromPattern)
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
    , patternSize = ( 2, 2 )
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


testPlane : TextPlane
testPlane =
    makeTextPlane (4, 4)
        (
            "0123" ++
            "4567" ++
            "89AB" ++
            "CDEF"
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
        -- --------------------------
        , hr [] []
        , testPlane |> viewRotationsAndFlips
        , hr [] []
        , testPlane |> viewSubPlanes
        , hr [] []
        , testPlane |> viewAllViews
        , hr [] []
        , testPlane |> viewPatterns
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


{-- --------- --}


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
                                [ style "display" "inline-block"
                                , style "width" "21px" ]
                                [ text <| String.fromChar c ]
                        )
                    row)
            )
        |> div [ style "flex" "column" ]


displayTextInBounds : Vec2 -> String -> Html Msg
displayTextInBounds (width, height) string =
    string
        |> splitBy width
        |> List.map (String.toList)
        |> displayCharGrid


displayTextPlane : TextPlane -> Html Msg
displayTextPlane plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault '?')
        |> displayCharGrid


viewRotationsAndFlips : TextPlane -> Html Msg
viewRotationsAndFlips plane =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
        [ displayTextPlane plane
        , text "North"
        , displayTextPlane <| rotate North plane
        , text "West"
        , displayTextPlane <| rotate West plane
        , text "South"
        , displayTextPlane <| rotate South plane
        , text "East"
        , displayTextPlane <| rotate East plane
        , text "Horz"
        , displayTextPlane <| flip Horizontal plane
        , text "Vert"
        , displayTextPlane <| flip Vertical plane
        ]


viewSubPlanes : TextPlane -> Html Msg
viewSubPlanes plane =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
        [ displayTextPlane plane
        , text "(0, 0) (2, 2)"
        , displayTextPlane <| WFC.Plane.sub (2, 2) plane
        , text "(0, 0) (3, 3)"
        , displayTextPlane <| Plane.sub (3, 3) plane
        , text "(1, 1) (2, 2)"
        , displayTextPlane <| Plane.subAt (1, 1) (2, 2) plane
        , text "(1, 1) (3, 3)"
        , displayTextPlane <| Plane.subAt (1, 1) (3, 3) plane
        , text "(0, 1) (3, 3)"
        , displayTextPlane <| Plane.subAt (0, 1) (3, 3) plane
        , text "(0, 1) (2, 3)"
        , displayTextPlane <| Plane.subAt (0, 1) (2, 3) plane
        , text "(3, 3) (1, 1)"
        , displayTextPlane <| Plane.subAt (3, 3) (1, 1) plane
        , text "(3, 3) (4, 4)"
        , displayTextPlane <| Plane.subAt (3, 3) (4, 4) plane
        ]


viewAllViews : TextPlane -> Html Msg
viewAllViews plane =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
    <| List.map displayTextPlane
    <| allViews plane


viewPatterns : TextPlane -> Html Msg
viewPatterns plane =
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-evenly"
        ]
    <| List.indexedMap
        (\index pattern ->
            div
                [ class <| "pattern-" ++ String.fromInt index
                , style "margin" "10px 0"
                ]
                [ span [] [ text <| String.fromInt index ]
                , displayTextPlane <| fromPattern pattern
                ]
        )
        (WFC.findPatterns (2, 2) plane)
