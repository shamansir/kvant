port module Mehanik exposing (..)


import Browser
import Http
import Dict exposing (Dict)
import Array exposing (Array)
import Bytes exposing (Bytes)
import Json.Encode as E
import Image exposing (Image)
import Image.Color as ImageC


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as H
import Html.Events exposing (..)

import Example.Instance.Text.Render as TextRenderer
import Example.Instance.Image.Render as ImageRenderer
import Example.Instance.Tiles.Render as TilesRenderer
import Example.Instance.Tiles exposing (TilingRules(..), toIndexInSet, fromIndexInSet)
import Example.Instance.Text.Plane as TextPlane
import Example.Instance.Image.Plane as ImagePlane exposing
    (colorToPixel, pixelToColor)
import Example.Instance.Tiles.Plane exposing (TileKey)
import Example.Instance.Tiles.Plane as TilesPlane
import Example.Instance.Tiles.Rules as Rules
import Example.Render exposing (Renderer)

import Kvant.Vec2 exposing (Vec2)
import Kvant.Plane as Plane
import Kvant.Plane as Plane exposing (Boundary(..), Symmetry(..))
import Kvant.Solver.Options exposing (Approach(..))
import Kvant.Solver.Options as Solver
import Kvant.Json.Options as Options
import Kvant.Patterns exposing (UniquePatterns)


type alias Model =
    { status : Status
    , example : CurrentExample
    , images : Dict ImageAlias (Result Http.Error Image)
    , rules : Dict TileGroup (Result String TilingRules)
    }


type Status
    = None
    | WaitingRunResponse
    | WaitingTracingResponse
    | Tracing


type CurrentExample
    = NotSelected
    | Textual
        { options : Solver.Options
        , source : ( Vec2, String )
        , wave : Maybe (Grid Char)
        , patterns : Maybe UniquePatterns
        }
    | FromImage
        { options : Solver.Options
        , source : Image
        , wave : Maybe Image
        , patterns : Maybe UniquePatterns
        }
    | FromTiles
        { options : Solver.Options
        , group : TileGroup
        , rules : TilingRules
        , wave : Maybe (Grid TileKey)
        , patterns : Maybe UniquePatterns
        }


type alias Grid a = Array (Array (Array a))


type alias ImageAlias = String


type alias TileGroup = String


type Msg
    = NoOp
    -- switch to example
    | SwitchToTextExample ( Vec2, String )
    | SwitchToImageExample Image
    | SwitchToTiledExample TileGroup
    -- controls for worker
    | Run
    | Trace
    | Step
    | StepBack
    | Stop
    -- receiving from Http requests
    | GotImage ImageAlias (Result Http.Error Image)
    | GotRules TileGroup (Result String TilingRules)
    -- receiving from worker
    | GotResult (Grid Int)
    -- change options
    | ChangeN Plane.Size
    | ChangeSymmetry Symmetry
    | ChangeOutputWidth Int
    | ChangeOutputHeight Int
    | UsePeriodicInput
    | UseBoundedInput
    | UsePeriodicOutput
    | UseBoundedOutput


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
    { status = None
    , example = NotSelected
    , images = Dict.empty
    , rules = Dict.empty
    }


defaultTextOptions =
    { approach =
        Overlapping
            { inputBoundary = Bounded -- Periodic
            , patternSize = ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    }


defaultImageOptions =
    { approach =
        Overlapping
            { inputBoundary = Bounded -- Periodic
            , patternSize = ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    }


defaultTilesOptions =
    { approach =
        Overlapping
            { inputBoundary = Bounded -- Periodic
            , patternSize = ( 2, 2 )
            , symmetry = NoSymmetry
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

        Run ->
            case model.example of

                NotSelected ->
                    ( model, Cmd.none )

                Textual { options, source }  ->
                    (
                        { model
                        | status = WaitingRunResponse
                        }
                    , runInWorker
                        { options =
                            Options.encode options
                        , source =
                            source
                                |> TextPlane.boundedStringToGrid
                                |> List.map (List.map Char.toCode)
                                |> List.map Array.fromList
                                |> Array.fromList
                        }
                    )

                FromImage { options, source } ->
                    (
                        { model
                        | status = WaitingRunResponse
                        }
                    , runInWorker
                        { options =
                            Options.encode options
                        , source =
                            source
                                |> ImageC.toArray2d
                                |> Array.map (Array.map colorToPixel)
                        }
                    )

                FromTiles { options, rules } ->
                    (
                        { model
                        | status = WaitingRunResponse
                        }
                    , case rules of
                        FromGrid tileSet grid ->
                            runInWorker
                                { options =
                                    Options.encode options
                                , source =
                                    grid
                                        |> Array.map (Array.map <| toIndexInSet tileSet)
                                }
                        FromRules _ -> Cmd.none
                    )

        Trace ->
            case model.example of

                NotSelected ->
                    ( model, Cmd.none )

                Textual { options, source } ->
                    (
                        { model
                        | status = WaitingTracingResponse
                        }
                    ,
                        traceInWorker
                            { options =
                                Options.encode options
                            , source =
                                source
                                    |> TextPlane.boundedStringToGrid
                                    |> List.map (List.map Char.toCode)
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            }
                    )

                FromImage { options, source } ->
                    (
                        { model
                        | status = WaitingTracingResponse
                        }
                    , traceInWorker
                        { options =
                            Options.encode options
                        , source =
                            source
                                |> ImageC.toArray2d
                                |> Array.map (Array.map colorToPixel)
                        }
                    )

                FromTiles { options, rules } ->
                    (
                        { model
                        | status = WaitingTracingResponse
                        }
                    , case rules of
                        FromGrid tileSet grid ->
                            runInWorker
                                { options =
                                    Options.encode options
                                , source =
                                    grid
                                        |> Array.map (Array.map <| toIndexInSet tileSet)
                                }
                        FromRules _ -> Cmd.none
                    )

        Step ->
            (
                { model
                | status = WaitingTracingResponse
                }
            , stepInWorker ()
            )

        StepBack ->
            (
                { model
                | status = WaitingTracingResponse
                }
            , stepBackInWorker ()
            )

        Stop ->
            (
                { model
                | status = None
                , example =
                    case model.example of
                        NotSelected ->
                            NotSelected
                        Textual state ->
                            Textual
                                { state
                                | wave = Nothing
                                }
                        FromImage state ->
                            FromImage
                                { state
                                | wave = Nothing
                                }
                        FromTiles state ->
                            FromTiles
                                { state
                                | wave = Nothing
                                }
                }
            , stopWorker ()
            )

        SwitchToTextExample source ->
            (
                { model
                | example =
                    Textual
                        { options = defaultTextOptions
                        , source = source
                        , wave = Nothing
                        , patterns = Nothing
                        }
                }
            , Cmd.none
            )

        SwitchToImageExample image ->
            (
                { model
                | example =
                    FromImage
                        { options = defaultImageOptions
                        , source = image
                        , wave = Nothing
                        , patterns = Nothing
                        }
                }
            , Cmd.none
            )

        SwitchToTiledExample group ->
            (
                { model
                | example =
                    case model.rules |> Dict.get group of
                        Just (Ok rules) ->
                            FromTiles
                                { options = defaultTilesOptions
                                , group = group
                                , rules = rules
                                , wave = Nothing
                                , patterns = Nothing
                                }
                        _ ->
                            model.example
                }
            , Cmd.none
            )

        ChangeN n ->
            (
                { model
                | example = changeOptions (changeN n) model.example
                }
            , Cmd.none
            )

        ChangeSymmetry symmetry ->
            (
                { model
                | example = changeOptions (changeSymmetry symmetry) model.example
                }
            , Cmd.none
            )

        ChangeOutputWidth w ->
            (
                { model
                | example = changeOptions (changeOutputSize <| \(_, h) -> (w, h)) model.example
                }
            , Cmd.none
            )

        ChangeOutputHeight h ->
            (
                { model
                | example = changeOptions (changeOutputSize <| \(w, _) -> (w, h)) model.example
                }
            , Cmd.none
            )

        UsePeriodicInput ->
            (
                { model
                | example = changeOptions (changeInputBoundary Periodic) model.example
                }
            , Cmd.none
            )

        UseBoundedInput ->
            (
                { model
                | example = changeOptions (changeInputBoundary Bounded) model.example
                }
            , Cmd.none
            )

        UsePeriodicOutput ->
            (
                { model
                | example = changeOptions (changeOutputBoundary Periodic) model.example
                }
            , Cmd.none
            )

        UseBoundedOutput ->
            (
                { model
                | example = changeOptions (changeOutputBoundary Bounded) model.example
                }
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
                {- if tileGroup == "Kotlin" then
                    Task.succeed "Kotlin"
                        |> Task.perform SwitchToTiledExample
                else -} Cmd.none
                -- FIXME
            )

        GotResult grid ->
            (
                { model
                | status =
                    case model.status of
                        None -> None
                        WaitingRunResponse -> None
                        WaitingTracingResponse -> Tracing
                        Tracing -> Tracing
                , example
                    = case model.example of

                        NotSelected ->
                            model.example

                        Textual state ->
                            Textual
                                { state
                                | wave =
                                    grid
                                        |> Array.map
                                            (Array.map <| Array.map Char.fromCode)
                                        |> Just
                                }

                        FromImage state ->
                            FromImage
                                { state
                                | wave =
                                    grid
                                        |> Array.map
                                            (Array.map
                                                <| Array.map pixelToColor
                                                    >> Array.toList
                                                    >> ImagePlane.merge)
                                        |> ImageC.fromArray2d
                                        |> Just
                                }

                        FromTiles state ->
                            FromTiles
                                <| case state.rules of
                                    FromGrid tileSet _ ->
                                        { state
                                        | wave =
                                            grid
                                            |> Array.map
                                                (Array.map
                                                    <| Array.map
                                                    <| fromIndexInSet tileSet
                                                )
                                            |> Just
                                        }
                                    FromRules _ ->
                                        state  -- FIXME: TODO
                }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let

        viewExample example =
            case example of

                Textual { source, wave } ->
                    div
                        []
                        [ viewSource TextRenderer.make source
                        , hr [] []
                        , wave
                            |> Maybe.map
                                (viewGrid <| Array.toList >> TextPlane.merge >> TextRenderer.char)
                            |> Maybe.withDefault (div [] [])
                        ]

                FromImage { source, wave } ->
                    div
                        []
                        [ viewSource ImageRenderer.make source
                        , hr [] []
                        , wave
                            |> Maybe.map ImageRenderer.drawImage
                            |> Maybe.withDefault (div [] [])
                        ]
                    --Example.view ImageRenderer.make exampleModel

                FromTiles { group, rules, wave } ->
                    case tiledExamples |> Dict.get group of
                        Just ( tiles, format ) ->
                            div []
                                [ div
                                    []
                                    <| List.map TilesRenderer.tile
                                    <| List.map (toTileUrl format group)
                                    <| tiles
                                , hr [] []
                                , case rules of
                                    FromGrid _ grid ->
                                        div
                                            [ style "transform" "scale(0.6)"
                                            , style "transform-origin" "0 0"
                                            ]
                                            [ viewSource
                                                (TilesRenderer.make <| toTileUrl format group)
                                                grid
                                            , wave
                                                |> Maybe.map
                                                    (TilesRenderer.grid1
                                                        <| Array.toList
                                                            >> TilesPlane.merge
                                                            >> TilesRenderer.tileAndCount
                                                                    (toTileUrl format group)
                                                    )
                                                |> Maybe.withDefault (div [] [])
                                            ]
                                    _ -> div [] []
                                ]

                        Nothing -> div [] []
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
                [input
                    [ type_ "radio"
                    , onClick msg
                    , checked isChecked
                    , disabled <| case model.status of
                        None -> False
                        _ -> True
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

        controlPanel title items =
            div [ style "display" "flex"
                , style "padding" "5px"
                , style "margin" "5px"
                , style "border" "1px solid black"
                , style "border-radius" "3px"
                , style "width" "fit-content"
                ]
                (
                    span
                        [ style "font-size" "10px"
                        , style "color" "white"
                        , style "background" "gray"
                        , style "padding" "3px"
                        , style "border-radius" "3px"
                        , style "max-height" "1em"
                        ]
                        [ Html.text title ]
                    :: items
                )

        controls options  =
            case options.approach of
                Overlapping { patternSize, inputBoundary, symmetry } ->

                    div
                        []
                        [ controlPanel "Pattern size"
                            [ checkbox
                                (case patternSize of (n, _) -> n == 2)
                                "2x"
                                <| ChangeN (2, 2)
                            , checkbox
                                (case patternSize of (n, _) -> n == 3)
                                 "3x"
                                <| ChangeN (3, 3)
                            ]
                        , controlPanel "Input"
                            [ checkbox
                                (case inputBoundary of
                                    Periodic -> True
                                    Bounded -> False)
                                "Periodic"
                                UsePeriodicInput
                            , checkbox
                                (case inputBoundary of
                                    Periodic -> False
                                    Bounded -> True)
                                "Bounded"
                                UseBoundedInput
                            ]
                        , controlPanel "Symmetry"
                            [ checkbox
                                (case symmetry of
                                    NoSymmetry -> True
                                    _ -> False)
                                "None"
                                <| ChangeSymmetry NoSymmetry
                            , checkbox
                                (case symmetry of
                                    FlipOnly -> True
                                    _ -> False)
                                "Only flip"
                                <| ChangeSymmetry FlipOnly
                            , checkbox
                                (case symmetry of
                                    RotateOnly -> True
                                    _ -> False)
                                "Only rotate"
                                <| ChangeSymmetry RotateOnly
                            , checkbox
                                (case symmetry of
                                    FlipAndRotate -> True
                                    _ -> False)
                                "Flip and rotate"
                                <| ChangeSymmetry FlipAndRotate
                            ]
                        , controlPanel "Output"
                            [ checkbox
                                (case options.outputBoundary of
                                    Periodic -> True
                                    Bounded -> False)
                                "Periodic"
                                UsePeriodicOutput
                            , checkbox
                                (case options.outputBoundary of
                                    Periodic -> False
                                    Bounded -> True)
                                "Bounded"
                                UseBoundedOutput
                            ]
                        , controlPanel "Output size"
                            [ span
                                []
                                [ Html.text
                                    <| "Width ("
                                        ++ (String.fromInt <| Tuple.first <| options.outputSize)
                                        ++ ")"]
                            , input
                                [ type_ "range"
                                , H.min "8"
                                , H.max "96"
                                , value <| String.fromInt <| Tuple.first <| options.outputSize
                                , onInput
                                    <| String.toInt >> Maybe.withDefault 8 >> ChangeOutputWidth
                                ]
                                []
                            , span
                                []
                                [ Html.text
                                    <| "Height ("
                                        ++ (String.fromInt <| Tuple.second <| options.outputSize)
                                        ++ ")"]
                            , input
                                [ type_ "range"
                                , H.min "8"
                                , H.max "96"
                                , value <| String.fromInt <| Tuple.second <| options.outputSize
                                , onInput <|
                                    String.toInt >> Maybe.withDefault 8 >> ChangeOutputHeight
                                ]
                                []
                            ]
                        , controlPanel "" -- "Run/Trace"
                            [ fancyButton
                                (model.status == None)
                                "▶️"
                                Run
                            , fancyButton
                                (model.status /= WaitingRunResponse
                                    && model.status /= WaitingTracingResponse
                                )
                                "⏭️"
                                (case model.status of
                                    Tracing -> Step
                                    None -> Trace
                                    _ -> NoOp
                                )
                            , fancyButton
                                (model.status == Tracing)
                                "⏮️"
                                StepBack
                            , fancyButton
                                (model.status == Tracing)
                                "⏹️"
                                Stop
                            ]
                        ]
                _ -> div [] []

        toTileUrl format group tile =
            "http://localhost:3000/tiled/" ++ group ++ "/" ++ tile ++ "." ++ format

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
                        (\boundedStr ->
                            exampleFrame (SwitchToTextExample boundedStr)
                                [ viewSource TextRenderer.make boundedStr
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
                Textual { options } -> controls options
                FromImage { options } -> controls options
                FromTiles { options } -> controls options
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
        , subscriptions =
            \_ ->
                gotWorkerResult GotResult
        , update = update
        , view = \model -> { title = "Kvant : Mehanik", body = [ view model ] }
        }


mapGrid : (a -> b) -> Grid a -> Grid b
mapGrid f = Array.map <| Array.map <| Array.map f


viewSource
    :  Renderer fmt a (Html msg)
    -> fmt
    -> Html msg -- Example.Msg
viewSource ( renderSource, _ ) source =
    renderSource source


viewGrid : (Array a -> Html msg) -> Array (Array (Array a)) -> Html msg
viewGrid viewElem =
    Array.map Array.toList >> Array.toList >> TilesRenderer.grid viewElem


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


changeN : Plane.Size -> Solver.Options -> Solver.Options
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

changeSymmetry : Symmetry -> Solver.Options -> Solver.Options
changeSymmetry symmetry options =
    { options
    | approach =
        case options.approach of
            Overlapping overlappingOpts ->
                Overlapping
                    { overlappingOpts
                    | symmetry = symmetry
                    }
            _ -> options.approach
    }


changeInputBoundary : Boundary -> Solver.Options -> Solver.Options
changeInputBoundary boundary options =
    { options
    | approach =
        case options.approach of
            Overlapping overlappingOpts ->
                Overlapping
                    { overlappingOpts
                    | inputBoundary = boundary
                    }
            _ -> options.approach
    }


changeOutputBoundary : Boundary -> Solver.Options -> Solver.Options
changeOutputBoundary boundary options =
    { options
    | outputBoundary = boundary
    }


changeOutputSize : (Plane.Size -> Plane.Size) -> Solver.Options -> Solver.Options
changeOutputSize f options =
    { options
    | outputSize = f options.outputSize
    }


changeOptions : (Solver.Options -> Solver.Options) -> CurrentExample -> CurrentExample
changeOptions f current =
    case current of
        Textual state ->
            Textual { state | options = f state.options }
        FromImage state ->
            FromImage { state | options = f state.options }
        FromTiles state ->
            FromTiles { state | options = f state.options }
        _ -> current


port runInWorker : { options : E.Value, source : Array (Array Int) } -> Cmd msg

port traceInWorker : { options : E.Value, source : Array (Array Int) } -> Cmd msg

port stepInWorker : () -> Cmd msg

port stepBackInWorker : () -> Cmd msg

port stopWorker : () -> Cmd msg

port gotWorkerResult : (Array (Array (Array Int)) -> msg) -> Sub msg
