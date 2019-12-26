module Main exposing (..)


import Browser

import Random
import Task
import Time
import Dict

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Core exposing (WFC, TextWFC)
import WFC.Core as WFC
import WFC.Plane exposing (..)
import WFC.Occured exposing (Occured(..))
import WFC.Plane as Plane exposing (sub ,subAt)
import WFC.Solver exposing (Approach(..), fromPattern)
import WFC.Solver as WFC exposing (TextOptions)


type alias Model =
    { source: String
    , result: Maybe String
    }


type Msg
    = NoOp
    | Calculate Random.Seed WFC.Instance



options : WFC.TextOptions
options =
    { approach = Overlapping
    , patternSearch = Bounded
    , patternSize = N ( 2, 2 )
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
        Calculate seed wfcInstance ->
            (
                { model
                | result =
                    case wfcInstance of
                        WFC.Text wfc ->
                            Just ( wfc |> WFC.run seed model.source )
                }
            , Cmd.none
            )


testPlane : TextPlane
testPlane =
    makeTextPlane (4, 4)
        (
            "0000" ++
            "0111" ++
            "0121" ++
            "0111"
        )


testPlaneHex : TextPlane
testPlaneHex =
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
            |> viewTextInBounds options.inputSize
        , hr [] []
        , model.result
            |> Maybe.map (viewTextInBounds options.outputSize)
            |> Maybe.withDefault (div [] [])
        -- --------------------------
        , hr [] []
        , hr [] []
        , hr [] []
        , testPlaneHex |> viewMaterialized
        , hr [] []
        , testPlaneHex |> rotate |> viewMaterialized
        , hr [] []
        , testPlaneHex |> rotate |> flip |> viewMaterialized
        , hr [] []
        , testPlane |> viewRotationsAndFlips
        , hr [] []
        , testPlaneHex |> viewRotationsAndFlips
        , hr [] []
        , testPlane |> viewSubPlanes
        , hr [] []
        , testPlaneHex |> viewSubPlanes
        , hr [] []
        , testPlane |> viewPeriodicSubPlanes
        , hr [] []
        , testPlaneHex |> viewPeriodicSubPlanes
        , hr [] []
        , testPlane |> viewAllViews
        , hr [] []
        , testPlaneHex |> viewAllViews
        , hr [] []
        , testPlane |> viewPatterns
        , hr [] []
        , testPlaneHex |> viewPatterns
        -- , hr [] []
        -- , testPlane |> viewAllSubPlanes
        -- , hr [] []
        -- , testPlaneHex |> viewAllSubPlanes
        ]


main : Program {} Model Msg
main =
    Browser.application
        { init = \_ _ _ ->
                    ( init,
                     Task.perform
                        (\time ->
                            Calculate
                                (Random.initialSeed <| Time.posixToMillis time)
                                <| WFC.Text <| WFC.text options
                        )
                        Time.now
                    )
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


viewChar : Char -> Html Msg
viewChar c =
    span
        [ style "display" "inline-block"
        , style "width" "9px"
        , style "background-color" <| symbolBg c
        , style "padding" "2px 8px"
        , style "color" <|
            if symbolBg c == "black" then
                "rgba(255,255,255,0.3)"
            else
                "rgba(0,0,0,0.3)"
        ]
        [ text <| String.fromChar c ]


viewGrid : (a -> Html Msg) -> List (List a) -> Html Msg
viewGrid viewElem grid =
    grid
        |> List.map
            (\row ->
                div [ style "display" "flex", style "flex-direction" "row" ]
                    <| List.map viewElem row
            )
        |> div [ style "display" "flex", style "flex-direction" "column" ]


viewPlane : a -> (a -> Html Msg) -> Plane Vec2 a -> Html Msg
viewPlane default viewElem plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault default)
        |> viewGrid viewElem


-- viewPlaneWith : a -> (Vec2 -> a -> Html Msg) -> Plane Vec2 a -> Html Msg
-- viewPlaneWith default viewElem plane =
--     foldMap plane
--         |> List.map (List.map <| Maybe.withDefault default)
--         |> viewGrid viewElem


viewTextInBounds : Vec2 -> String -> Html Msg
viewTextInBounds (width, height) string =
    string
        |> splitBy width
        |> List.map (String.toList)
        |> viewGrid viewChar


viewTextPlane : TextPlane -> Html Msg
viewTextPlane = viewPlane '?' viewChar


viewRotationsAndFlips : TextPlane -> Html Msg
viewRotationsAndFlips plane =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
        [ viewTextPlane plane
        , text "North"
        , viewTextPlane <| rotateTo North plane
        , text "West"
        , viewTextPlane <| rotateTo West plane
        , text "South"
        , viewTextPlane <| rotateTo South plane
        , text "East"
        , viewTextPlane <| rotateTo East plane
        , text "Horz"
        , viewTextPlane <| flipBy Horizontal plane
        , text "Vert"
        , viewTextPlane <| flipBy Vertical plane
        ]

        {-
        , text "rotate once"
        , viewTextPlane <| rotate plane
        , text "rotate twice"
        , viewTextPlane <| rotate <| rotate plane
        , text "rotate triple times"
        , viewTextPlane <| rotate <| rotate <| rotate plane
        , text "flip"
        , viewTextPlane <| flip plane
        , text "flip rotated"
        , viewTextPlane <| flip <| rotate plane
        , text "rotate flipped"
        , viewTextPlane <| rotate <| flip plane
        -}


viewSubPlanes : TextPlane -> Html Msg
viewSubPlanes plane =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
        [ viewTextPlane plane
        , text "(0, 0) (2, 2)"
        , Plane.sub (N (2, 2)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        , text "(0, 0) (3, 3)"
        , Plane.sub (N (3, 3)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        , text "(1, 1) (2, 2)"
        , Plane.subAt (1, 1) (N (2, 2)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        , text "(1, 1) (3, 3)"
        , Plane.subAt (1, 1) (N (3, 3)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        , text "(0, 1) (3, 3)"
        , Plane.subAt (0, 1) (N (3, 3)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        , text "(0, 1) (2, 3)"
        , Plane.subAt (0, 1) (N (2, 3)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        , text "(3, 3) (1, 1)"
        , Plane.subAt (3, 3) (N (1, 1)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        , text "(3, 3) (4, 4)"
        , Plane.subAt (3, 3) (N (4, 4)) plane
            |> Maybe.map viewTextPlane
            |> Maybe.withDefault (text "<NONE>")
        ]


viewPeriodicSubPlanes : TextPlane -> Html Msg
viewPeriodicSubPlanes plane =
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-evenly"
        ]
        [ viewTextPlane plane
        , text "(0, 0) (2, 2)"
        , Plane.periodicSubAt (0, 0) (N (2, 2)) plane
            |> viewTextPlane
        , text "(0, 0) (3, 3)"
        , Plane.periodicSubAt (0, 0) (N (3, 3)) plane
            |> viewTextPlane
        , text "(1, 1) (2, 2)"
        , Plane.periodicSubAt (1, 1) (N (2, 2)) plane
            |> viewTextPlane
        , text "(1, 1) (3, 3)"
        , Plane.periodicSubAt (1, 1) (N (3, 3)) plane
            |> viewTextPlane
        , text "(0, 1) (3, 3)"
        , Plane.periodicSubAt (0, 1) (N (3, 3)) plane
            |> viewTextPlane
        , text "(0, 1) (2, 3)"
        , Plane.periodicSubAt (0, 1) (N (2,3)) plane
            |> viewTextPlane
        , text "(3, 3) (1, 1)"
        , Plane.periodicSubAt (3, 3) (N (1, 1)) plane
            |> viewTextPlane
        , text "(3, 3) (4, 4)"
        , Plane.periodicSubAt (3, 3) (N (4, 4)) plane
            |> viewTextPlane
        , text "(2, 3) (4, 4)"
        , Plane.periodicSubAt (2, 3) (N (4, 4)) plane
            |> viewTextPlane
        , text "(-2, -2) (4, 4)"
        , Plane.periodicSubAt (-2, -2) (N (4, 4)) plane
            |> viewTextPlane
        ]


viewAllSubPlanes : TextPlane -> Html Msg
viewAllSubPlanes plane =
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-evenly"
        ]
        <| List.indexedMap viewWithIndex
        <| List.map viewTextPlane
        <| findAllSubs options.patternSearch options.patternSize plane


viewWithIndex : Int -> Html Msg -> Html Msg
viewWithIndex index subView =
    div []
        [ text <| String.fromInt index ++ "."
        , subView
        ]


viewAllViews : TextPlane -> Html Msg
viewAllViews plane =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
    <| List.map viewTextPlane
    <| allViews plane


viewPatterns : TextPlane -> Html Msg
viewPatterns plane =
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-evenly"
        ]
    <| Dict.values
    <| Dict.map
        (\index {occured, pattern, matches} ->
            div
                [ class <| "pattern-" ++ String.fromInt index
                , style "margin" "10px 0"
                ]
                [ span [] [ text <| String.fromInt index ++ ". " ]
                , span [] [ text <| occursText occured ]
                , viewTextPlane <| fromPattern pattern
                , span [] [ text <| "Matches: " ]
                , viewMatches matches
                ]
        )
        (WFC.findUniquePatterns options.patternSearch (N (2, 2)) plane)


viewMatches : Plane (Offset Vec2) (List Int) -> Html Msg
viewMatches plane =
    let
        itemSpan = span [ style "padding" "5px" ]
    in
        disregardOffsets plane
            |> viewPlane []
                (\matchesList ->
                    div [ style "width" "100px" ] <|
                        List.map
                            (String.fromInt
                                >> text
                                >> List.singleton
                                >> itemSpan)
                        matchesList
                )



viewMaterialized : TextPlane -> Html Msg
viewMaterialized plane =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
        <| List.map viewCell
        <| materializeFlatten plane


-- viewMatches :


viewCell : Cell Vec2 Char -> Html Msg
viewCell ( (x, y), char ) =
    div
        []
        [ text <| "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ "):"
        , text <| Maybe.withDefault "%" <| Maybe.map String.fromChar <| char
        ]


occursText : Occured -> String
occursText occured =
    case occured of
        Unknown -> "occurs unknown amount of times"
        Times howMuch -> "occurs " ++ String.fromInt howMuch ++ " times"


symbolBg : Char -> String
symbolBg symbol =
    case symbol of
        '0' -> "white"
        '1' -> "black"
        '2' -> "red"
        '3' -> "aqua"
        '4' -> "blue"
        '5' -> "green"
        '6' -> "salmon"
        '7' -> "maroon"
        '8' -> "#85C1E9"
        '9' -> "#5D6D7E"
        'A' -> "#5DADE2"
        'B' -> "#F9E79F"
        'C' -> "#F4D03F"
        'D' -> "#E74C3C"
        'E' -> "#BFC9CA"
        'F' -> "#2E86C1"
        _ -> "lightgray"
