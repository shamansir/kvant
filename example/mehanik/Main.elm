module Main exposing (..)


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

import Example.Main exposing (..)
import Example.Main as Example exposing (..)
import Example.Msg as Example exposing (Msg)
import Example.Render.Html.Make as FlatExample exposing (..)
import Example.Render as Render exposing (..)
import Example.Render.Html.Flat as Render exposing (..)
import Example.Render.Html.Grid as Render exposing (..)

import Example.Instance.Text as TextExample exposing (..)
import Example.Instance.Image as ImageExample exposing (..)
import Example.Instance.Pixels as PixelsExample exposing (..)
import Example.Instance.Text.RenderHtml as Render exposing (..)
import Example.Instance.Text.RenderHtml as Text exposing (spec)
import Example.Instance.Image.RenderHtml as Render exposing (..)
import Example.Instance.Image.RenderHtml as Image exposing (spec)
import Example.Instance.Text.Plane exposing (TextPlane)
import Example.Instance.Text.Plane as Text exposing (toGrid)
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
    , images : Dict ImageAlias Image
    }


type CurrentExample
    = NotSelected
    | WaitingForImage ImageAlias
    | Textual Wfc.TextOptions TextExample
    | FromImage Wfc.ImageOptions Image ImageExample
    | FromPixels Wfc.ImageOptions Pixels PixelsExample


type alias ImageAlias = String

type alias Pixels = Array (Array Color)

type Msg
    = NoOp
    | ToExample Example.Msg
    | LoadTextExample Vec2 String
    | LoadImageExample ImageAlias
    | ChangeN (N Vec2)
    | GotImage ImageAlias (Result Http.Error Image)
    | GotPixels Pixels


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
    ]


imagesForOverlap =
    [ "3Bricks"
    , "Angular"
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
    ]


init : Model
init =
    { example = NotSelected
    , images = Dict.empty
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

        LoadTextExample size source ->
            (
                { model
                | example =
                    Textual defaultTextOptions
                        <| initExpands
                        <| TextExample.quick defaultTextOptions ( size, source )
                }
            , Cmd.none
            )

        LoadImageExample imageAlias ->
            (
                { model
                | example = WaitingForImage imageAlias
                }
            , requestImages [ imageAlias ]
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
                                    <| initExpands
                                    <| ImageExample.quick newOptions image
                        }
                _ -> model
            , Cmd.none )

        GotImage gotAlias result ->
            ( case result of
                Ok image ->
                    { model
                    | images =
                        model.images
                            |> Dict.insert gotAlias image
                    , example =
                        case model.example of
                            WaitingForImage waitingForAlias ->
                                if waitingForAlias == gotAlias then
                                    FromImage defaultImageOptions image
                                        <| initExpands
                                        <| ImageExample.quick defaultImageOptions image
                                else model.example
                            _ -> model.example
                    }
                Err _ -> model
            , Cmd.none
            )

        GotPixels pixels ->
            (
                { model
                | example =
                    FromPixels defaultImageOptions pixels
                        <| initExpands
                        <| PixelsExample.quick defaultImageOptions pixels
                }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        textRenderer =
            FlatExample.make (\(size, src) -> Text.toGrid size src) Text.spec
        imageRenderer =
            FlatExample.make ImageC.toList2d Image.spec
        pixelsRenderer =
            FlatExample.make (Array.toList >> List.map Array.toList) Image.spec
        viewImageExample =
            Example.view ToExample imageRenderer
        viewImage image =
            Render.grid Render.pixel <| ImageC.toList2d image
        viewExample =
            case model.example of
                Textual _ exampleModel -> Example.view ToExample textRenderer exampleModel
                FromImage _ image exampleModel ->
                    div
                        []
                        [ viewImage image
                        , Example.view ToExample imageRenderer exampleModel
                        ]
                FromPixels _ pixels exampleModel ->
                    div
                        []
                        [ pixels
                            |> Array.toList
                            |> List.map Array.toList
                            |> Render.grid Render.pixel
                        , Example.view ToExample pixelsRenderer exampleModel
                        ]
                NotSelected -> Html.text "Not Selected"
                WaitingForImage url -> Html.text <| "Waiting for image " ++ url ++ " to load"
        makeTextOptions =
            textExamples
                |> List.map
                    (\source ->
                        option
                            [ value <| "t" ++ source ]
                            [ Html.text source ]
                    )
        makeImagesOptions =
            imagesForOverlap
                |> List.map
                    (\imageName ->
                        option
                            [ value <| "i" ++ imageName ]
                            [ Html.text imageName ]
                    )
        makeOptions -- There's `optgroup` !
            =  [ option [ {- disabled True -} ] [ Html.text "Select example" ] ]
            ++ [ option [ disabled True ] [ Html.text "-----" ] ]
            ++ makeTextOptions
            ++ [ option [ disabled True ] [ Html.text "-----" ] ]
            ++ makeImagesOptions
        selectionToMsg s =
            case s |> String.toList of
                marker::rest ->
                    case marker of
                        'i' -> LoadImageExample <| String.fromList rest
                        't' -> LoadTextExample (4, 4) <| String.fromList rest
                        _ -> NoOp
                _ -> NoOp
        controls options  =
            case options.approach of
                Overlapping { patternSize } ->

                    div []
                        [ input
                            [ type_ "number"
                            , H.min "2"
                            , H.max "4"
                            , value <| String.fromInt <| case patternSize of N ( n, _ ) -> n
                            , onInput
                                (String.toInt
                                    >> Maybe.withDefault 2
                                    >> (\n -> (n, n))
                                    >> N
                                    >> ChangeN
                                )
                            ] []
                        ]

                _ -> div [] []
    in
        div
            []
            [ select
                [ onInput selectionToMsg ]
                makeOptions
            , case model.example of
                Textual options _ -> controls options
                FromImage options _ _ -> controls options
                _ -> Html.text ""
            , viewExample
            ]


main : Program {} Model Msg
main =
    Browser.application
        { init =
            \_ _ _ ->
                ( init
                , requestImages [ "Hogs" ]
                )
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "Kvant", body = [ view model ] }
        }


requestImage : ImageAlias -> Cmd Msg
requestImage imageAlias =
    Http.get
        { url = "http://localhost:3000/overlapping/" ++ imageAlias ++ ".png"
        , expect =
            Http.expectBytesResponse
                (GotImage imageAlias)
                loadImage
        }


requestImages : List ImageAlias -> Cmd Msg
requestImages = List.map requestImage >> Cmd.batch


loadImage : Http.Response Bytes -> Result Http.Error (Image)
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

