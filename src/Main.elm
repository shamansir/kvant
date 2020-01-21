module Main exposing (..)


import Browser

import Random
import Task
import Time
-- import Dict
import Dict exposing (Dict)
import Dict as D exposing (map)
import Array exposing (Array)
import Array as A exposing (map)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Render.Core as Render exposing (..)
import Render.Flat as Render exposing (..)
import Render.Text as Render exposing (..)
import Render.Text as Text exposing (spec)
import Render.Grid as Render exposing (..)
import Render.Example as Example exposing (..)
import Render.Example.Flat as FlatExample exposing (..)

import Color exposing (Color)
import Image exposing (Image)

import WFC.Core exposing (WFC, TextWFC, TextTracingWFC, TextTracingPlane)
import WFC.Core as WFC
import WFC.Vec2 exposing (..)
import WFC.Plane exposing (Plane, N(..))
import WFC.Plane.Flat as Plane exposing (SearchMethod(..))
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
    }


type ExampleKind
    = Text
    | Image


type Msg
    = NoOp
    | WithExample ExampleKind ExampleId ExampleMsg


textOptions : WFC.Options Vec2
textOptions =
    { approach = Overlapping
    , patternSearch = Bounded -- Periodic
    , patternSize = N ( 2, 2 )
    , inputSize = ( 4, 4 )
    , outputSize = ( 10, 10 )
    -- , advanceRule = WFC.MaximumAttempts 50
    , advanceRule = WFC.AdvanceManually
    }


init : Model
init =
    let
        quickTextExample src size =
            { source = src
            , sourcePlane = TextPlane.make size src
            , options = textOptions
            , expands = []
            , wfc =
                ( WFC.text textOptions src
                , WFC.textTracing textOptions src
                )
            , status = None
            }
            |> initExpands
    in
        { textExamples =
            [ quickTextExample
                (
                    "AAAA" ++
                    "ABBA" ++
                    "ABBA" ++
                    "AAAA"
                )
                (4, 4)
            , quickTextExample
                (
                    "0000" ++
                    "0111" ++
                    "0121" ++
                    "0111"
                )
                (4, 4)
            , quickTextExample
                (
                    "0123" ++
                    "4567" ++
                    "89AB" ++
                    "CDEF"
                )
                (4, 4)
            ]
        , imageExamples = []
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
                    Text ->
                        injectUpdates
                            model.textExamples
                            (\newExamples -> { model | textExamples = newExamples })
                    Image ->
                        injectUpdates
                            model.imageExamples
                            (\newExamples -> { model | imageExamples = newExamples })


view : Model -> Html Msg
view model =
    let
        textRenderer =
            FlatExample.make Text.toGrid Text.spec
        viewTextExample index =
            Example.view (WithExample Text index) textRenderer
        viewImageExample index _ = div [] [] -- FIXME: implement
    in
        div
            [ ]
            <| (model.textExamples |> List.indexedMap viewTextExample)
            ++ (model.imageExamples |> List.indexedMap viewImageExample)


main : Program {} Model Msg
main =
    Browser.application
        { init = \_ _ _ -> ( init, Cmd.none )
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions = always Sub.none
        , update = update
        , view = \model -> { title = "WFC", body = [ view model ] }
        }


