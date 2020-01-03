module View exposing (..)


import Dict

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import WFC.Vec2 exposing (..)
import WFC.Plane.Plane exposing (N(..), Plane, Cell)
import WFC.Occurrence exposing (Occurrence(..), frequencyToFloat)
import WFC.Plane.Flat as Plane exposing (SearchMethod(..), sub ,subAt, foldMap)
import WFC.Plane.Flat exposing (..)
import WFC.Plane.Text exposing (TextPlane)
import WFC.Plane.Offset exposing (Offset, OffsetPlane)
import WFC.Plane.Offset as Offsets exposing (foldMap)
import WFC.Solver as WFC


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


viewChar : Char -> Html msg
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


viewCoord : ( Int, Int ) -> Html msg
viewCoord ( x, y ) =
    span
        [ style "position" "absolute"
        , style "font-size" "7px"
        , style "background-color" "lightgray"
        , style "padding" "2px"
        , style "border-radius" "7px"
        , style "opacity" "0.5"
        ]
        [ text <| "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")" ]


viewGrid : (a -> Html msg) -> List (List a) -> Html msg
viewGrid viewElem grid =
    grid
        |> List.map
            (\row ->
                div [ style "display" "flex", style "flex-direction" "row" ]
                    <| List.map viewElem row
            )
        |> div [ style "display" "flex", style "flex-direction" "column" ]


viewGridV : (v -> a -> Html msg) -> List (List (v, a)) -> Html msg
viewGridV viewElem grid =
    viewGrid (\(v, a) -> viewElem v a) grid -- a.k.a. `uncurry`


viewPlane : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
viewPlane default viewElem plane =
    unpack plane
        |> List.map (List.map <| Maybe.withDefault default)
        |> viewGrid viewElem


viewPlaneWith : a -> (Vec2 -> a -> Html msg) -> Plane Vec2 a -> Html msg
viewPlaneWith default viewElem plane =
    Plane.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default |> Tuple.pair v)
        plane
        |> viewGridV viewElem


viewOffsetPlane : a -> (a -> Html msg) -> OffsetPlane Vec2 a -> Html msg
viewOffsetPlane default viewElem plane =
    Offsets.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default)
        plane
        |> viewGrid viewElem


viewOffsetPlaneWith : a -> (Vec2 -> a -> Html msg) -> OffsetPlane Vec2 a -> Html msg
viewOffsetPlaneWith default viewElem plane =
    Offsets.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default |> Tuple.pair v)
        plane
        |> viewGridV viewElem


viewTextInBounds : Vec2 -> String -> Html msg
viewTextInBounds (width, height) string =
    string
        |> splitBy width
        |> List.map (String.toList)
        |> viewGrid viewChar


viewTextPlane : TextPlane -> Html msg
viewTextPlane =
    viewPlaneWith '?'
        <| \coord char ->
            span
                []
                [ viewCoord coord
                , viewChar char
                ]


viewRotationsAndFlips : TextPlane -> Html msg
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


viewSubPlanes : TextPlane -> Html msg
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


viewPeriodicSubPlanes : TextPlane -> Html msg
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


viewAllSubPlanes : Plane.SearchMethod -> N Vec2 -> TextPlane -> Html msg
viewAllSubPlanes method n plane =
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-evenly"
        ]
        <| List.indexedMap viewWithIndex
        <| List.map viewTextPlane
        <| findAllSubsAlt method n plane


viewWithIndex : Int -> Html msg -> Html msg
viewWithIndex index subView =
    div []
        [ text <| String.fromInt index ++ "."
        , subView
        ]


viewAllViews : TextPlane -> Html msg
viewAllViews plane =
    div [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
    <| List.map viewTextPlane
    <| allViews plane


viewPatterns : Plane.SearchMethod -> N Vec2 -> TextPlane -> Html msg
viewPatterns method n plane =
    let
        patterns = WFC.findUniquePatterns method n plane
    in
    div [ style "display" "flex"
        , style "flex-direction" "column"
        , style "justify-content" "space-evenly"
        ]
        <| Dict.values
        <| Dict.map
            (\index {frequency, pattern, matches} ->
                div
                    [ class <| "pattern-" ++ String.fromInt index
                    , style "margin" "10px 0"
                    ]
                    [ span [] [ text <| String.fromInt index ++ ". " ]
                    , span [] [ text <| (occursText <| Tuple.first frequency) ++ ". " ]
                    , span [] [ text <| "frequency: " ++
                        (case Tuple.second frequency of
                            Just freq -> freq |> frequencyToFloat |> String.fromFloat
                            Nothing -> "unknown") ]
                    , viewTextPlane pattern
                    , span [] [ text <| "Matches: " ]
                    -- , viewMatches matches
                    , matches |> viewMatchesWithPatterns patterns
                    ]
            )
            patterns


viewMatches : OffsetPlane Vec2 (List Int) -> Html msg
viewMatches plane =
    let
        itemSpan =
            span
                [ style "padding" "3px"
                ]
    in
        plane
            |> viewOffsetPlane []
                (\matchesList ->
                    div
                        [ style "height" "50px"
                        , style "width" "50px"
                        , style "border" "1px dashed gray"
                        , style "display" "flex"
                        , style "flex-wrap" "wrap"
                        , style "font-size" "9px"
                        ]
                        <| List.map
                            (String.fromInt
                                >> text
                                >> List.singleton
                                >> itemSpan)
                            matchesList
                )


viewMatchesWithPatterns : WFC.UniquePatterns Vec2 Char -> OffsetPlane Vec2 (List Int) -> Html msg
viewMatchesWithPatterns patterns plane =
    let
        patternWrapper patternData =
            div
                [ style "display" "inline-block"
                , style "padding" "1px"
                , style "transform" "scale(0.4,0.6)"
                , style "border" "1px solid"
                , style "max-width" "50px"
                , style "margin-right" "-20px"
                ]
                [ viewTextPlane patternData.pattern
                ]
    in
        plane
            |> viewOffsetPlaneWith []
                (\coord matchesList ->
                    div [ style "height" "130px"
                        , style "width" "130px"
                        , style "border" "1px dashed gray"
                        ]
                        [ viewCoord coord
                        , div
                            [ style "display" "flex"
                            , style "flex-direction" "row"
                            , style "align-items" "start"
                            , style "flex-wrap" "wrap"
                            , style "font-size" "9px"
                            ]
                            <| List.map
                                (\match ->
                                    patterns
                                        |> Dict.get match
                                        |> Maybe.map patternWrapper
                                        |> Maybe.withDefault (div [] [ text "<NO>" ])
                                )
                                matchesList
                        ]
                )


viewMaterialized : TextPlane -> Html msg
viewMaterialized plane =
    div
        [ style "display" "flex"
        , style "flex-direction" "row"
        , style "justify-content" "space-evenly"
        ]
        <| List.map viewCell
        <| materializeFlatten plane


-- viewMatches :


viewCell : Cell Vec2 Char -> Html msg
viewCell ( coord, char ) =
    div
        []
        [ viewCoord coord
        , text <| Maybe.withDefault "%" <| Maybe.map String.fromChar <| char
        ]


occursText : Occurrence -> String
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


viewStepStatus : WFC.Step Vec2 -> Html msg
viewStepStatus (WFC.Step num _ status) =
    span
        []
        [ text <| String.fromInt num
        , text " "
        , text <| case status of
           WFC.Initial -> "(initial)"
           WFC.InProgress _ -> "(in progress)"
           WFC.Solved _ -> "(solved)"
           WFC.Terminated -> "(terminated)"
           WFC.Exceeded attempts -> "(exceeded " ++ String.fromInt attempts ++ ")"
        ]
