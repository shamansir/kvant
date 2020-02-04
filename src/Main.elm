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
    { textExamples : List TextExample
    , imageExamples : List ImageExample
    , images : Dict String Image
    }


type ExampleKind
    = TextExample
    | ImageExample


type alias Url = String

type Msg
    = NoOp
    | WithExample ExampleKind ExampleId ExampleMsg
    | GotImage Url (Result Http.Error Image)


init : Model
init =
    { textExamples =
        [ TextExample.quick
            (
                "AAAA" ++
                "ABBA" ++
                "ABBA" ++
                "AAAA"
            )
            (4, 4)
        , TextExample.quick
            (
                "0000" ++
                "0111" ++
                "0121" ++
                "0111"
            )
            (4, 4)
        , TextExample.quick
            (
                "0123" ++
                "4567" ++
                "89AB" ++
                "CDEF"
            )
            (4, 4)
        ]
    , imageExamples = []
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

        WithExample kind requestedId exampleMsg ->

            let

                updateIfEqual index example =
                    if index == requestedId then
                        let
                            ( newExampleModel, commands ) =
                                Example.update requestedId exampleMsg example
                        in
                            ( newExampleModel
                            , commands |> Cmd.map (WithExample kind index)
                            )
                    else
                        ( example, Cmd.none )

                injectUpdates examples inject =
                    let
                        updates
                            = examples |> List.indexedMap updateIfEqual
                    in
                        ( inject (updates |> List.map Tuple.first)
                        , updates
                            |> List.map Tuple.second
                            |> Cmd.batch
                        )

            in

                case kind of
                    TextExample ->
                        injectUpdates
                            model.textExamples
                            (\newExamples -> { model | textExamples = newExamples })
                    ImageExample ->
                        injectUpdates
                            model.imageExamples
                            (\newExamples -> { model | imageExamples = newExamples })

        GotImage url result ->
            ( case result of
                Ok image ->
                    { model
                    | images =
                        model.images
                            |> Dict.insert url image
                    , imageExamples =
                        ImageExample.quick image
                            :: model.imageExamples
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
        viewTextExample index =
            Example.view (WithExample TextExample index) textRenderer
        viewImageExample index =
            Example.view (WithExample ImageExample index) imageRenderer
        viewImage index ( url, image ) =
            div
                []
                [ text <| String.fromInt index ++ ". " ++ url
                , Render.grid Render.pixel <| ImageC.toList2d image
                ]
    in
        div
            [ ]
            <| (model.images |> Dict.toList |> List.indexedMap viewImage)
            ++ (model.imageExamples |> List.indexedMap viewImageExample)
            ++ (model.textExamples |> List.indexedMap viewTextExample)


main : Program {} Model Msg
main =
    Browser.application
        { init =
            \_ _ _ ->
                ( init
                , requestImages
                    [ "Hogs.png" ]
                )
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "WFC", body = [ view model ] }
        }


requestImage : String -> Cmd Msg
requestImage fileName =
    Http.get
        { url = "http://localhost:3000/samples/" ++ fileName
        , expect =
            Http.expectBytesResponse
                (GotImage fileName)
                loadImage
        }


requestImages : List String -> Cmd Msg
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
