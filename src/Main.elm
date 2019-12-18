module Main exposing (..)


import Browser

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Core exposing (WFC, TextWFC)
import WFC.Core as WFC
import WFC.Plane exposing (..)
import WFC.Plane as Plane exposing (sub ,subAt)
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
        , let
            testRotationPlane =
                makeTextPlane (4, 4) (
                    "0123" ++
                    "4567" ++
                    "89AB" ++
                    "CDEF"
                )

        in
            div [ style "display" "flex"
                , style "flex-direction" "row"
                , style "justify-content" "space-evenly"
                ]
                [ displayTextPlane testRotationPlane
                , text "North"
                , displayTextPlane <| rotate North testRotationPlane
                , text "West"
                , displayTextPlane <| rotate West testRotationPlane
                , text "South"
                , displayTextPlane <| rotate South testRotationPlane
                , text "East"
                , displayTextPlane <| rotate East testRotationPlane
                , text "Horz"
                , displayTextPlane <| flip Horizontal testRotationPlane
                , text "Vert"
                , displayTextPlane <| flip Vertical testRotationPlane
                ]
        , hr [] []
        , let
            testSubsPlane =
                makeTextPlane (4, 4) (
                    "0123" ++
                    "4567" ++
                    "89AB" ++
                    "CDEF"
                )

        in
            div [ style "display" "flex"
                , style "flex-direction" "row"
                , style "justify-content" "space-evenly"
                ]
                [ displayTextPlane testSubsPlane
                , text "(0, 0) (2, 2)"
                , displayTextPlane <| WFC.Plane.sub (2, 2) testSubsPlane
                , text "(0, 0) (3, 3)"
                , displayTextPlane <| Plane.sub (3, 3) testSubsPlane
                , text "(1, 1) (2, 2)"
                , displayTextPlane <| Plane.subAt (1, 1) (2, 2) testSubsPlane
                , text "(1, 1) (3, 3)"
                , displayTextPlane <| Plane.subAt (1, 1) (3, 3) testSubsPlane
                , text "(0, 1) (3, 3)"
                , displayTextPlane <| Plane.subAt (0, 1) (3, 3) testSubsPlane
                , text "(0, 1) (2, 3)"
                , displayTextPlane <| Plane.subAt (0, 1) (2, 3) testSubsPlane
                , text "(3, 3) (1, 1)"
                , displayTextPlane <| Plane.subAt (3, 3) (1, 1) testSubsPlane
                , text "(3, 3) (4, 4)"
                , displayTextPlane <| Plane.subAt (3, 3) (4, 4) testSubsPlane
                ]
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
