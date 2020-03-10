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
import Html.Events exposing (..)

import Render.Core as Render exposing (..)
import Render.Flat as Render exposing (..)
import Render.Grid as Render exposing (..)
import Render.Text as Render exposing (..)
import Render.Text as Text exposing (spec)
import Render.Image as Render exposing (..)
import Render.Image as Image exposing (spec)
import Render.Example as Example exposing (..)
import Render.Example.Flat as FlatExample exposing (..)
import Render.Example.Text as TextExample exposing (..)
import Render.Example.Image as ImageExample exposing (..)

import WFC.Core exposing (WFC, TextWFC, TextTracingWFC, TextTracingPlane)
import WFC.Core as WFC
import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Plane, N(..))
import WFC.Plane.Flat as Plane exposing (Boundary(..))
import WFC.Plane.Flat exposing (flip, rotate)
import WFC.Plane.Impl.Text exposing (TextPlane)
import WFC.Plane.Impl.Text as Text exposing (toGrid)
import WFC.Plane.Impl.Text as TextPlane exposing (make)
import WFC.Plane.Impl.Tracing exposing (TracingPlane)
import WFC.Solver exposing (Approach(..))
import WFC.Solver as WFC exposing (Step, Options)
import WFC.Solver.History as H exposing (..)


type alias Model =
    { example : CurrentExample
    , images : Dict ImageAlias Image
    }


type CurrentExample
    = NotSelected
    | WaitingForImage ImageAlias
    | Textual TextExample
    | FromImage Image ImageExample


type alias ImageAlias = String

type Msg
    = NoOp
    | ToExample ExampleMsg
    | LoadTextExample Vec2 String
    | LoadImageExample ImageAlias
    | GotImage ImageAlias (Result Http.Error Image)


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
                Textual example ->
                    let
                        ( newExampleModel, commands ) =
                            Example.update exampleMsg example
                    in
                        (
                            { model
                            | example = Textual newExampleModel
                            }
                        , commands |> Cmd.map ToExample
                        )
                FromImage image example ->
                    let
                        ( newExampleModel, commands ) =
                            Example.update exampleMsg example
                    in
                        (
                            { model
                            | example = FromImage image newExampleModel
                            }
                        , commands |> Cmd.map ToExample
                        )
                _ -> ( model, Cmd.none )

        LoadTextExample size source ->
            (
                { model
                | example = Textual <| TextExample.quick source size
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
                                    FromImage image <| ImageExample.quick image
                                else model.example
                            _ -> model.example
                    }
                Err _ -> model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        textRenderer =
            FlatExample.make (\(size, src) -> Text.toGrid size src) Text.spec
        imageRenderer =
            FlatExample.make ImageC.toList2d Image.spec
        viewImageExample =
            Example.view ToExample imageRenderer
        viewImage image =
            Render.grid Render.pixel <| ImageC.toList2d image
        viewExample =
            case model.example of
                Textual exampleModel -> Example.view ToExample textRenderer exampleModel
                FromImage image exampleModel ->
                    div
                        []
                        [ viewImage image
                        , Example.view ToExample imageRenderer exampleModel
                        ]
                NotSelected -> text "Not Selected"
                WaitingForImage url -> text <| "Waiting for image " ++ url ++ " to load"
        makeTextOptions =
            textExamples
                |> List.map
                    (\source ->
                        option
                            [ value <| "t" ++ source ]
                            [ text source ]
                    )
        makeImagesOptions =
            imagesForOverlap
                |> List.map
                    (\imageName ->
                        option
                            [ value <| "i" ++ imageName ]
                            [ text imageName ]
                    )
        makeOptions -- There's `optgroup` !
            =  [ option [ {- disabled True -} ] [ text "Select example" ] ]
            ++ [ option [ disabled True ] [ text "-----" ] ]
            ++ makeTextOptions
            ++ [ option [ disabled True ] [ text "-----" ] ]
            ++ makeImagesOptions
        selectionToMsg s =
            case s |> String.toList of
                marker::rest ->
                    case marker of
                        'i' -> LoadImageExample <| String.fromList rest
                        't' -> LoadTextExample (4, 4) <| String.fromList rest
                        _ -> NoOp
                _ -> NoOp
    in
        div
            []
            [ select
                [ onInput selectionToMsg ]
                makeOptions
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
        , view = \model -> { title = "WFC", body = [ view model ] }
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
