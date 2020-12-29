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
import Example.Instance.Text.Plane exposing (TextPlane)
import Example.Instance.Text.Plane as TextPlane exposing (make)


import Kvant.Core as Wfc
import Kvant.Vec2 exposing (..)
import Kvant.Plane exposing (Plane, N(..))
import Kvant.Plane.Flat as Plane exposing (Boundary(..), Symmetry(..))
import Kvant.Plane.Flat exposing (flip, rotate)
import Kvant.Plane.Impl.Tracing exposing (TracingPlane)
import Kvant.Solver exposing (Approach(..))
import Kvant.Solver as Solver exposing (Step, Options)
import Kvant.Solver.History as H exposing (..)


type alias Model =
    { example : CurrentExample
    , images : Dict ImageAlias (Result Http.Error Image)
    , tiles : Dict TileGroup (Dict TileAlias (Result Http.Error Image))
    }


type CurrentExample
    = NotSelected
    | Textual Wfc.TextOptions TextExample
    | FromImage Wfc.ImageOptions Image ImageExample
    | FromTiles Wfc.TilesOptions TilesRegistry TilesExample


type alias ImageAlias = String


type alias TileGroup = String


type alias TileAlias = String

type alias TileKey = ( TileGroup, TileAlias )


type alias Pixels = Array (Array Color)


type Msg
    = NoOp
    | ToExample Example.Msg
    | SwitchToTextExample Vec2 String
    | SwitchToImageExample Image
    | SwitchToTiledExample TileGroup
    | ChangeN (N Vec2)
    | GotImage ImageAlias (Result Http.Error Image)
    | GotTile TileKey (Result Http.Error Image)
    -- | GotPixels Pixels


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


imagesForOverlap =
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


tiledForOverlap =
    [
        ( "Castle"
        , [ "bridge", "ground", "river", "riverturn", "road", "roadturn", "t", "tower", "wall", "wallriver", "wallroad" ]
        )
    ,
        ( "Circles"
        , [ "b_half", "b_i", "b_quarter", "b", "w_half", "w_i", "w_quarter", "w" ]
        )

    ]


init : Model
init =
    { example = NotSelected
    , images = Dict.empty
    , tiles = Dict.empty
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
                | example = model.example
                    {- case rules of
                        FromGrid tilesGrid ->
                            FromTiles defaultTilesOptions registry
                                <| TilesExample.quick defaultTilesOptions tilesGrid
                        FromRules _ ->
                            -- FIXME
                            model.example -}
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

        GotTile ( tileGroup, tileAlias ) result ->
            (
                { model
                | tiles =
                    model.tiles
                        |> Dict.update tileGroup
                            (\maybeGroupDict ->
                                Just
                                    <| Dict.insert tileAlias result
                                    <| case maybeGroupDict of
                                        Just groupDict -> groupDict
                                        Nothing -> Dict.empty
                            )
                }
            , Cmd.none
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
                    div
                        []
                        [ Example.view ImageRenderer.make exampleModel
                        ]
                FromTiles _ registry exampleModel ->
                    div
                        []
                        [ {- Example.view ImageRenderer.make exampleModel
                            |> Html.map ToExample -}
                        ]
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
                    (tiledForOverlap
                    |> List.map
                        (\(group, files) ->
                            case model.tiles |> Dict.get group of
                                Just tilesDict ->
                                    case files |> List.head of
                                        Just firstImage ->
                                            imageFrom
                                                (\_ -> SwitchToTiledExample group)
                                                tilesDict
                                                firstImage
                                        Nothing ->
                                            exampleFrame NoOp
                                                [ Html.text <| group ++ ": No images" ]
                                Nothing ->
                                    exampleFrame NoOp
                                        [ Html.text <| group ++ ": Loading..." ]
                        )
                    )
                ++
                    (imagesForOverlap
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
                _ -> Html.text ""
            , viewExample model.example
            ]


main : Program {} Model Msg
main =
    Browser.application
        { init =
            \_ _ _ ->
                ( init
                , Cmd.batch
                    [ requestImages imagesForOverlap
                    , requestTiles tiledForOverlap
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


requestTile : TileKey -> Cmd Msg
requestTile ( tileGroup, tileAlias ) =
    Http.get
        { url = "http://localhost:3000/tiled/" ++ tileGroup ++ "/" ++ tileAlias ++ ".png"
        , expect =
            Http.expectBytesResponse
                (GotTile ( tileGroup, tileAlias ))
                loadImage
        }


requestImages : List ImageAlias -> Cmd Msg
requestImages = List.map requestImage >> Cmd.batch



requestTiles : List ( TileGroup, List TileAlias ) -> Cmd Msg
requestTiles =
    List.concatMap
        (\(group, tiles) -> tiles |> List.map (Tuple.pair group))
        >> List.map requestTile
        >> Cmd.batch


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

