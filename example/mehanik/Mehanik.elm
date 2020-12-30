module Mehanik exposing (..)


import Browser
import Http

import Random
import Task
import Time
-- import Dict
import Dict exposing (Dict)
import Dict as D exposing (map)
import Array exposing (Array)
import Array as A exposing (map)
import Bytes.Decode as Decode exposing (..)
import Bytes.Decode as Bytes exposing (Decoder)
import Bytes exposing (Bytes)

import Color exposing (Color)
import Image exposing (Image)
import Image.Color as ImageC exposing (toList2d)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as H exposing (min, max)
import Html.Events exposing (..)

import Example.Instance exposing (Instance(..))
import Example.Instance.Text as Wfc exposing (TextOptions)
import Example.Instance.Image as Wfc exposing (ImageOptions)
import Example.Instance.Tiles as Wfc exposing (TilesOptions)
import Example.Advance exposing (..)

import Example.Main exposing (..)
import Example.Main as Example exposing (..)
import Example.Msg as Example exposing (Msg)
import Example.Render as Render exposing (..)

import Example.Instance.Text as TextExample exposing (..)
import Example.Instance.Image as ImageExample exposing (..)
import Example.Instance.Tiles as TilesExample exposing (..)
import Example.Instance.Text.Render as TextRenderer exposing (make)
import Example.Instance.Image.Render as ImageRenderer exposing (make)
import Example.Instance.Tiles.Render as TilesRenderer exposing (grid)
import Example.Instance.Text.Plane exposing (TextPlane)
import Example.Instance.Text.Plane as TextPlane exposing (make)
import Example.Instance.Tiles.Plane exposing (TileKey)
import Example.Instance.Tiles.Rules as Rules


import Kvant.Core as Wfc
import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Plane, N(..))
import Kvant.Plane.Flat as Plane exposing (Boundary(..), Symmetry(..))
import Kvant.Plane.Flat exposing (flip, rotate)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step, Options)
import Kvant.Solver.History as H exposing (..)
import List.Extra exposing (group)


type alias Model =
    { example : CurrentExample
    , images : Dict ImageAlias (Result Http.Error Image)
    , rules : Dict TileGroup (Result String TilingRules)
    }


type CurrentExample
    = NotSelected
    | Textual Wfc.TextOptions TextExample
    | FromImage Wfc.ImageOptions Image ImageExample
    | FromTiles Wfc.TilesOptions TileGroup TilesExample


type alias ImageAlias = String


type alias TileGroup = String


type alias Pixels = Array (Array Color)


type Msg
    = NoOp
    | ToExample Example.Msg
    | SwitchToTextExample Vec2 String
    | SwitchToImageExample Image
    | SwitchToTiledExample TileGroup
    | ChangeN (N Vec2)
    | GotImage ImageAlias (Result Http.Error Image)
    | GotRules TileGroup (Result String TilingRules)


textExamples =
    [
        "AAAA" ++
        "ABBA" ++
        "ABBA" ++
        "AAAA"
    ,
        "0000" ++
        "0111" ++
        "0121" ++
        "0111"
    ,
        "0123" ++
        "4567" ++
        "89AB" ++
        "CDEF"
    ,
        "Platformer"
    ,
        "ColoredCity"
    ,
        "Dungeon"
    ,
        "3Bricks"
    ]


pixelatedExamples =
    [ "Angular"
    , "Cat"
    , "Cats"
    , "Cave"
    , "Chess"
    , "City"
    , "ColoredCity"
    , "Dungeon"
    , "Fabric"
    , "Flowers"
    , "Forest"
    , "Hogs"
    , "Knot"
    , "Lake"
    , "LessRooms"
    , "Link"
    , "Link2"
    , "MagicOffice"
    , "Maze"
    , "Mazelike"
    , "MoreFlowers"
    , "Mountains"
    , "Nested"
    , "Office"
    , "Office2"
    , "Paths"
    , "Platformer"
    , "Qud"
    , "RedDot"
    , "RedMaze"
    , "Rooms"
    , "Rule126"
    , "ScaledMaze"
    , "Sewers"
    , "SimpleKnot"
    , "SimpleMaze"
    , "SimpleWall"
    , "Skew1"
    , "Skew2"
    , "Skyline"
    , "Skyline2"
    , "SmileCity"
    , "Spirals"
    , "Town"
    , "TrickKnot"
    , "Village"
    , "Water"
    , "3Bricks"
    ]


tiledExamples =
    Dict.fromList
        [
            ( "Kotlin"
            ,
                ( [ "filled_corner_bl", "filled_corner_br", "filled_corner_tl", "filled_corner_tr", "filled_quad", "mixed_corner_bl", "mixed_corner_bl", "mixed_corner_br", "mixed_corner_tl", "mixed_corner_tr", "mixed_corner_bl", "mixed_corner_tr", "striped_corner_bl", "striped_corner_br", "striped_corner_tl", "striped_corner_tr", "striped_quad_l", "striped_quad_r" ], "svg" )
            )
        {- ,
            ( "Castle"
            ,
                ( [ "bridge", "ground", "river", "riverturn", "road", "roadturn", "t", "tower", "wall", "wallriver", "wallroad" ], "png" )
            )
        ,
            ( "Circles"
            ,
                ( [ "b_half", "b_i", "b_quarter", "b", "w_half", "w_i", "w_quarter", "w" ], "png" )
            ) -}
        ]


init : Model
init =
    { example = NotSelected
    , images = Dict.empty
    , rules = Dict.empty
    }


defaultTextOptions =
    { approach =
        Overlapping
            { searchBoundary = Bounded -- Periodic
            , patternSize = N ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    }


defaultImageOptions =
    { approach =
        Overlapping
            { searchBoundary = Bounded -- Periodic
            , patternSize = N ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    }


defaultTilesOptions =
    { approach =
        Overlapping
            { searchBoundary = Bounded -- Periodic
            , patternSize = N ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    }


update
    :  Msg
    -> Model
    -> ( Model, Cmd Msg )
update msg model =
    case msg of

        NoOp ->
            ( model, Cmd.none )

        ToExample exampleMsg ->
            case model.example of
                -- FIXME: fix repitition
                Textual options example ->
                    let
                        ( newExampleModel, commands ) =
                            Example.update exampleMsg example
                    in
                        (
                            { model
                            | example = Textual options newExampleModel
                            }
                        , commands |> Cmd.map ToExample
                        )
                FromImage options image example ->
                    let
                        ( newExampleModel, commands ) =
                            Example.update exampleMsg example
                    in
                        (
                            { model
                            | example = FromImage options image newExampleModel
                            }
                        , commands |> Cmd.map ToExample
                        )
                FromTiles options group example ->
                    let
                        ( newExampleModel, commands ) =
                            Example.update exampleMsg example
                    in
                        (
                            { model
                            | example = FromTiles options group newExampleModel
                            }
                        , commands |> Cmd.map ToExample
                        )
                _ -> ( model, Cmd.none )

        SwitchToTextExample size source ->
            (
                { model
                | example =
                    Textual defaultTextOptions
                        <| TextExample.quick defaultTextOptions ( size, source )
                }
            , Cmd.none
            )

        SwitchToImageExample image ->
            (
                { model
                | example =
                    FromImage defaultImageOptions image
                        <| ImageExample.quick defaultImageOptions image
                }
            , Cmd.none
            )

        SwitchToTiledExample group ->
            (
                { model
                | example =
                    case model.rules |> Dict.get group of
                        Just _ ->
                            FromTiles defaultTilesOptions group
                                <| TilesExample.quick defaultTilesOptions
                                <| Array.empty -- FIXME
                        _ ->
                            model.example
                }
            , Cmd.none
            )

        ChangeN n ->
            ( case model.example of
                Textual options example ->
                    let newOptions = options |> changeN n
                    in
                        { model
                        | example =
                            Textual newOptions
                                <| TextExample.quick newOptions example.source
                        }
                FromImage options image example ->
                    let newOptions = options |> changeN n
                    in
                        { model
                        | example =
                            FromImage newOptions image
                                    <| ImageExample.quick newOptions image
                        }
                _ -> model
            , Cmd.none
            )

        GotImage gotAlias result ->
            (
                { model
                | images =
                    model.images
                        |> Dict.insert gotAlias result
                }
            , Cmd.none
            )

        GotRules tileGroup result ->
            (
                { model
                | rules =
                    model.rules
                        |> Dict.insert tileGroup result
                }
            ,
                if tileGroup == "Kotlin" then
                    Task.succeed "Kotlin"
                        |> Task.perform SwitchToTiledExample
                else Cmd.none
                -- FIXME
            )

view : Model -> Html Msg
view model =
    let

        viewImageExample =
            Example.view ImageRenderer.make >> Html.map ToExample

        viewExample example =
            case example of
                Textual _ exampleModel ->
                    Example.view TextRenderer.make exampleModel
                FromImage _ image exampleModel ->
                    Example.view ImageRenderer.make exampleModel
                FromTiles _ group exampleModel ->
                    case tiledExamples |> Dict.get group of
                        Just ( tiles, format ) ->
                            div []
                                [ div
                                    []
                                    <| List.map (tileToImage format group)
                                    <| tiles
                                , case model.rules |> Dict.get group of
                                    Just (Ok (FromGrid grid)) ->
                                        grid
                                            |> Array.map (Array.toList)
                                            |> Array.toList
                                            |> TilesRenderer.grid (tileToImage format group)
                                    _ -> div [] []
                                ]
                        Nothing -> div [] []
                    {- , Example.view ImageRenderer.make exampleModel
                        |> Html.map ToExample -}
                NotSelected -> Html.text "Not Selected"
                -- WaitingForImage url -> Html.text <| "Waiting for image " ++ url ++ " to load"

        fancyButton isEnabled label msg =
            button
                [ onClick msg
                , style "border" "none"
                , style "background" "none"
                , style "cursor" "pointer"
                , style "outline" "none"
                , style "opacity" <| if isEnabled then "1.0" else "0.5"
                ]
                [ Html.text label
                ]

        checkbox isChecked label msg =
            span
                []
                [ input
                    [ type_ "radio"
                    , onClick msg
                    , checked isChecked
                    , style "border"
                        <| if isChecked
                            then "1px solid aqua"
                            else "1px solid black"
                    , style "padding" "5px"
                    , style "margin" "5px"
                    , style "background" "none"
                    , style "cursor" "pointer"
                    , style "outline" "none"
                    , style "display" "inline-block"
                    , value label
                    ] []
                , Html.text label
                ]

        controlPanel =
            div [ style "display" "flex"
                , style "padding" "5px"
                , style "margin" "5px"
                , style "border" "1px solid black"
                , style "border-radius" "3px"
                , style "width" "fit-content"
                ]

        controls options  =
            case options.approach of
                Overlapping { patternSize } ->

                    div
                        []
                        [ controlPanel
                            [ fancyButton
                                (not <| isSolving model.example)
                                "▶️"
                                Example.TriggerRunning
                                |> Html.map ToExample
                            , fancyButton
                                True
                                "⏭️"
                                (if not <| isSolving model.example
                                    then Example.TriggerTracing
                                    else Example.NextStep
                                )
                                |> Html.map ToExample
                            , fancyButton
                                (isSolving model.example)
                                "⏮️"
                                Example.TriggerPreviousStep
                                |> Html.map ToExample
                            , fancyButton
                                (isSolving model.example)
                                "⏹️"
                                Example.Stop
                                |> Html.map ToExample
                            ]

                        , controlPanel
                            [ checkbox
                                (case patternSize of N (n, _) -> n == 2) "2x"
                                <| ChangeN <| N (2, 2)
                            , checkbox
                                (case patternSize of N (n, _) -> n == 3) "3x"
                                <| ChangeN <| N (3, 3)
                            ]

                        ]
                _ -> div [] []

        tileToImage format group tile =
            img
                [ src <| "http://localhost:3000/tiled/"
                    ++ group ++ "/" ++ tile
                    ++ "." ++ format
                , width 50
                , height 50
                ]
                [ ]

        imageFrom toMsg dict imgAlias =
            case dict |> Dict.get imgAlias of
                Just (Ok image) ->
                    exampleFrame (toMsg image)
                        [ img
                            [ src <| Image.toPngUrl image
                            , style "min-width" "50px"
                            , style "min-height" "50px"
                            , style "image-rendering" "pixelated"
                            ]
                            []
                        ]
                Just (Err error) ->
                    exampleFrame NoOp
                        [ Html.text <| imgAlias ++ ": Error " ++ errorToString error ]
                Nothing ->
                    exampleFrame NoOp
                        [ Html.text <| imgAlias ++ ": Loading..." ]

        exampleFrame msg =
            div
                [ style "margin" "5px"
                , style "padding" "10px"
                , style "border" "1px solid black"
                , style "border-radius" "5px"
                , style "background-color" "#f5f5f5"
                , style "cursor" "pointer"
                , onClick msg
                ]

        examples
            = div
                [ style "display" "flex"
                , style "overflow" "scroll"
                ]
                (
                    (textExamples
                    |> List.map (Tuple.pair (4, 4))
                    |> List.map
                        (\(size, str) ->
                            exampleFrame (SwitchToTextExample size str)
                                [ Example.preview TextRenderer.make (size, str)
                                ]
                        )
                    )
                ++
                    (tiledExamples
                    |> Dict.toList
                    |> List.map
                        (\( group, ( files, format ) ) ->
                            case model.rules |> Dict.get group of
                                Just _ ->
                                    exampleFrame (SwitchToTiledExample group)
                                        [ img
                                            [ src <| "http://localhost:3000/tiled/"
                                                ++ group ++ "/" ++
                                                (files |> List.head |> Maybe.withDefault "")
                                                ++ "." ++ format
                                            , width 50
                                            , height 50
                                            ]
                                            []
                                        ]
                                Nothing ->
                                    exampleFrame NoOp
                                        [ Html.text <| group ++ ": Loading..." ]
                        )
                    )
                ++
                    (pixelatedExamples
                    |> List.map
                        (imageFrom SwitchToImageExample model.images)
                    )
                )
    in
        div
            [ style "font-family" "sans-serif" ]
            [ examples
            {-, select
                [ onInput selectionToMsg ]
                makeOptions -}
            , case model.example of
                Textual options _ -> controls options
                FromImage options _ _ -> controls options
                FromTiles options _ _ -> controls options
                NotSelected -> Html.text ""
            , viewExample model.example
            ]


main : Program {} Model Msg
main =
    Browser.application
        { init =
            \_ _ _ ->
                ( init
                , Cmd.batch
                    [ requestAllImages pixelatedExamples
                    , requestAllRules <| Dict.keys tiledExamples
                    ]
                )
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "Kvant : Mehanik", body = [ view model ] }
        }


isSolving : CurrentExample -> Bool
isSolving example =
    case example of
        NotSelected -> False
        Textual _ e -> Example.Advance.isSolving e.status
        FromImage _ _ e -> Example.Advance.isSolving e.status
        FromTiles _ _ e -> Example.Advance.isSolving e.status



requestImage : ImageAlias -> Cmd Msg
requestImage imageAlias =
    Http.get
        { url = "http://localhost:3000/overlapping/" ++ imageAlias ++ ".png"
        , expect =
            Http.expectBytesResponse
                (GotImage imageAlias)
                loadImage
        }


requestAllImages : List ImageAlias -> Cmd Msg
requestAllImages = List.map requestImage >> Cmd.batch


requestRules : TileGroup -> Cmd Msg
requestRules tileGroup =
    Http.get
        { url = "http://localhost:3000/tiled/" ++ tileGroup ++ "/data.xml"
        , expect =
            Http.expectString
                (Result.mapError errorToString
                >> Result.andThen Rules.decode
                >> GotRules tileGroup
                )
        }


requestAllRules : List TileGroup -> Cmd Msg
requestAllRules = List.map requestRules >> Cmd.batch


loadImage : Http.Response Bytes -> Result Http.Error Image
loadImage response =
    case response of
        Http.BadUrl_ url ->
          Err (Http.BadUrl url)

        Http.Timeout_ ->
          Err Http.Timeout

        Http.NetworkError_ ->
          Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
          Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata bytes ->
          case Image.decode bytes of
            Just image ->
              Ok image

            Nothing ->
              Err (Http.BadBody "Image failed to be decoded")


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage


changeN : N Vec2 -> Solver.Options Vec2 a -> Solver.Options Vec2 a
changeN n options =
    { options
    | approach =
        case options.approach of
            Overlapping overlappingOpts ->
                Overlapping
                    { overlappingOpts
                    | patternSize = n
                    }
            _ -> options.approach
    }

