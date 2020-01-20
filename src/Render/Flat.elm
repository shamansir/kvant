module Render.Flat exposing (..)

import Dict

import Html exposing (..)
import Html.Attributes exposing (..)

import WFC.Plane exposing (..)
import WFC.Plane.Flat exposing (..)
import WFC.Plane.Flat as Plane exposing (SearchMethod(..), sub ,subAt, foldMap, unpack)
import WFC.Plane.Offset exposing (Offset, OffsetPlane)
import WFC.Plane.Offset as Offsets exposing (foldMap)
import WFC.Solver as WFC
import WFC.Solver.Flat as WFC
import WFC.Vec2 exposing (..)
import WFC.Occurrence exposing (frequencyToFloat)

import Render.Core as Render exposing (..)
import Render.Grid as Render exposing (..)


coord : ( Int, Int ) -> Html msg
coord ( x, y ) =
    span
        [ style "position" "absolute"
        , style "font-size" "7px"
        , style "background-color" "lightgray"
        , style "padding" "2px"
        , style "border-radius" "7px"
        , style "opacity" "0.5"
        ]
        [ text <| "(" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")" ]


step : WFC.Step Vec2 -> Html msg
step (WFC.Step num _ status) =
    span
        [ style "padding" "0 2px"
        ]
        [ text <| String.fromInt num
        -- , text " "
        -- , text seed
        , text " "
        , text <| case status of
           WFC.Initial -> "(initial)"
           WFC.InProgress focus _ ->
            "(in progress"
                ++ case focus of
                    WFC.FocusedAt (x, y) ->
                        ": (" ++ String.fromInt x ++ "," ++ String.fromInt y ++ "))"
                    WFC.NotFocused -> ")"
           WFC.Solved _ -> "(solved)"
           WFC.Terminated -> "(terminated)"
           WFC.Exceeded attempts -> "(exceeded " ++ String.fromInt attempts ++ ")"
        ]


plane : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
plane default viewElem =
    Plane.unpack
        >> List.map (List.map <| Maybe.withDefault default)
        >> Render.grid viewElem


planeV : a -> (Vec2 -> a -> Html msg) -> Plane Vec2 a -> Html msg
planeV default viewElem =
    Plane.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default |> Tuple.pair v)
        >> Render.gridV viewElem


withCoords : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
withCoords default viewElem =
    planeV default
        <| \theCoord elem ->
            span
                []
                [ coord theCoord
                , viewElem elem
                ]


-- TODO: join with the above
cell : a -> (a -> Html msg) -> Cell Vec2 a -> Html msg
cell default viewElem ( theCoord, v ) =
    span
        []
        [ coord theCoord
        , viewElem <| Maybe.withDefault default <| v
        ]


offsetPlane : a -> (a -> Html msg) -> OffsetPlane Vec2 a -> Html msg
offsetPlane default viewElem =
    Offsets.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default)
        >> grid viewElem


offsetPlaneV : a -> (Vec2 -> a -> Html msg) -> OffsetPlane Vec2 a -> Html msg
offsetPlaneV default viewElem =
    Offsets.foldMap
        (\(v, maybeVal) -> maybeVal |> Maybe.withDefault default |> Tuple.pair v)
        >> gridV viewElem


list : a -> (a -> Html msg) -> List ( String, Plane Vec2 a ) -> Html msg
list default viewElem  =
    Render.listBy
        (\( label, thePlane ) ->
            div []
                [ text label
                , thePlane |> withCoords default viewElem
                ]
        )


indexedList : a -> (a -> Html msg) -> List (Plane Vec2 a) -> Html msg
indexedList default viewElem planes =
    list default viewElem (planes |> List.indexedMap (String.fromInt >> Tuple.pair))


rotationsAndFlips : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
rotationsAndFlips default viewElem p =
    list default viewElem
        [ ( "Original", p )
        , ( "North", rotateTo North p )
        , ( "West", rotateTo West p )
        , ( "South", rotateTo South p )
        , ( "East", rotateTo East p )
        , ( "Horz", flipBy Horizontal p )
        , ( "Vert", flipBy Vertical p )
        , ( "rotate once", rotate p )
        , ( "rotate twice", rotate <| rotate p )
        , ( "rotate triple times", rotate <| rotate <| rotate p )
        , ( "flip", flip p )
        , ( "flip rotated", flip <| rotate p )
        , ( "rotate flipped", rotate <| flip p )
        ]


subPlanes : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
subPlanes default viewElem thePlane  =
    list default viewElem
        (
        [ ( "Original", Just thePlane )
        , ( "(0, 0) (2, 2)", Plane.sub (N (2, 2)) thePlane )
        , ( "(0, 0) (3, 3)", Plane.sub (N (3, 3)) thePlane )
        , ( "(1, 1) (2, 2)", Plane.subAt (1, 1) (N (2, 2)) thePlane )
        , ( "(1, 1) (3, 3)", Plane.subAt (1, 1) (N (3, 3)) thePlane )
        , ( "(0, 1) (3, 3)", Plane.subAt (0, 1) (N (3, 3)) thePlane )
        , ( "(0, 1) (2, 3)", Plane.subAt (0, 1) (N (2, 3)) thePlane )
        , ( "(3, 3) (1, 1)", Plane.subAt (3, 3) (N (1, 1)) thePlane )
        , ( "(3, 3) (4, 4)", Plane.subAt (3, 3) (N (4, 4)) thePlane )
        ] |> List.map (Tuple.mapSecond <| Maybe.withDefault thePlane)
        )

-- |> Maybe.map viewTextPlane
--             |> Maybe.withDefault (text "<NONE>")


periodicSubPlanes : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
periodicSubPlanes default viewElem thePlane  =
    list default viewElem
        (
        [ ( "Original", thePlane )
        , ( "(0, 0) (2, 2)", Plane.periodicSubAt (0, 0) (N (2, 2)) thePlane )
        , ( "(0, 0) (3, 3)", Plane.periodicSubAt (0, 0) (N (3, 3)) thePlane )
        , ( "(1, 1) (2, 2)", Plane.periodicSubAt (1, 1) (N (2, 2)) thePlane )
        , ( "(1, 1) (3, 3)", Plane.periodicSubAt (1, 1) (N (3, 3)) thePlane )
        , ( "(0, 1) (3, 3)", Plane.periodicSubAt (0, 1) (N (3, 3)) thePlane )
        , ( "(0, 1) (2, 3)", Plane.periodicSubAt (0, 1) (N (2, 3)) thePlane )
        , ( "(3, 3) (1, 1)", Plane.periodicSubAt (3, 3) (N (1, 1)) thePlane )
        , ( "(3, 3) (4, 4)", Plane.periodicSubAt (3, 3) (N (4, 4)) thePlane )
        , ( "(2, 3) (4, 4)", Plane.periodicSubAt (2, 3) (N (4, 4)) thePlane )
        , ( "(-2, -2) (4, 4)", Plane.periodicSubAt (-2, -2) (N (4, 4)) thePlane )
        ]
        )


viewAllSubPlanes : a -> (a -> Html msg) -> Plane.SearchMethod -> N Vec2 -> Plane Vec2 a -> Html msg
viewAllSubPlanes default viewElem method size =
    indexedList default viewElem
        << findAllSubsAlt method size


pattern
    :  a
    -> (a -> Html msg)
    -> WFC.UniquePatterns Vec2 a
    -> Int
    -> WFC.PatternWithStats Vec2 a
    -> Html msg
pattern default viewElem uniquePatterns index patternWithStats =
     div
        [ class <| "pattern-" ++ String.fromInt index
        , style "margin" "10px 0"
        ]
        [ span [] [ text <| String.fromInt index ++ ". " ]
        , span [] [ text <| (Render.occursText <| Tuple.first patternWithStats.frequency) ++ ". " ]
        , span [] [ text <| "frequency: " ++
            (case Tuple.second patternWithStats.frequency of
                Just freq -> freq |> frequencyToFloat |> String.fromFloat
                Nothing -> "unknown") ]
        , withCoords default viewElem patternWithStats.pattern
        , span [] [ text <| "Matches: " ]
        -- , viewMatches matches
        , patternWithStats.matches
            |> matchesWithPatterns default viewElem uniquePatterns
        ]


patterns : a -> (a -> Html msg) -> Plane.SearchMethod -> N Vec2 -> Plane Vec2 a -> Html msg
patterns default viewElem method n p =
    let
        uniquePatterns = WFC.findUniquePatterns method n p
    in
        div [ style "display" "flex"
            , style "flex-direction" "column"
            , style "justify-content" "space-evenly"
            ]
            <| Dict.values
            <| Dict.map
                (pattern default viewElem uniquePatterns)
                uniquePatterns


matches : OffsetPlane Vec2 (List Int) -> Html msg
matches thePlane =
    let
        itemSpan =
            span
                [ style "padding" "3px"
                ]
    in
        thePlane
            |> offsetPlane []
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


matchesWithPatterns
    :  a
    -> (a -> Html msg)
    -> WFC.UniquePatterns Vec2 a
    -> OffsetPlane Vec2 (List Int)
    -> Html msg
matchesWithPatterns default viewElem uniquePatterns thePlane =
    let
        patternWrapper patternId patternData =
            div
                [ style "display" "inline-block"
                , style "padding" "1px"
                , style "transform" "scale(0.4,0.6)"
                , style "border" "1px solid"
                , style "max-width" "50px"
                , style "margin-right" "-20px"
                ]
                [ withCoords default viewElem patternData.pattern
                , span
                    [ style "position" "absolute"
                    , style "padding-top" "5px"
                    , style "font-size" "1.3em"
                    ]
                    [ text <| String.fromInt patternId ]
                ]
    in
        thePlane
            |> offsetPlaneV []
                (\theCoord matchesList ->
                    div [ style "height" "130px"
                        , style "width" "130px"
                        , style "border" "1px dashed gray"
                        ]
                        [ coord theCoord
                        , div
                            [ style "display" "flex"
                            , style "flex-direction" "row"
                            , style "align-items" "start"
                            , style "flex-wrap" "wrap"
                            , style "font-size" "9px"
                            ]
                            <| List.map
                                (\match ->
                                    uniquePatterns
                                        |> Dict.get match
                                        |> Maybe.map (patternWrapper match)
                                        |> Maybe.withDefault (div [] [ text "<NO>" ])
                                )
                                matchesList
                        ]
                )


viewMaterialized : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
viewMaterialized default viewElem =
    listBy (cell default viewElem)
        << materializeFlatten


allViews : a -> (a -> Html msg) -> Plane Vec2 a -> Html msg
allViews default viewElem p =
    indexedList default viewElem <| Plane.allViews p
