port module Mehanik exposing (..)


import Browser
import Http
import Dict exposing (Dict)
import Array exposing (Array)
import Bytes exposing (Bytes)
import Json.Encode as JE
import Json.Decode as JD
import Xml.Decode as XD
import Image exposing (Image)
import Image.Color as ImageC


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as H
import Html.Events exposing (..)


import Kvant.Vec2 exposing (Vec2, loadSize)
import Kvant.Vec2 as Vec2
import Kvant.Plane as Plane
import Kvant.Plane as Plane exposing (Boundary(..), Symmetry(..))
import Kvant.Solver.Options exposing (Approach(..))
import Kvant.Solver.Options as Solver
import Kvant.Json.Options as Options
import Kvant.Patterns exposing (UniquePatterns)
import Kvant.Json.Patterns as Patterns
import Kvant.Neighbours exposing (Neighbours)
import Kvant.Neighbours as Neighbours
import Kvant.Neighbours as Dir exposing (Direction(..))
import Kvant.Matches exposing (Matches)
import Kvant.Matches as Matches
import Kvant.Json.Matches as Matches
import Kvant.Patterns exposing (PatternId)
import Kvant.Tiles exposing
    (toIndexInSet, fromIndexInSet, buildMapping, noMapping, TileMapping, TileKey, TileSet, Rotation)
import Kvant.Xml.Tiles as Tiles
import Kvant.Adjacency exposing (Adjacency(..))
import Kvant.Xml.Adjacency as Adjacency


import Example.Instance.Text.Render as TextRenderer
import Example.Instance.Image.Render as ImageRenderer
import Example.Instance.Tiles.Render as TilesRenderer
import Example.Instance.Text.Plane as TextPlane
import Example.Instance.Image.Plane as ImagePlane exposing
    (colorToPixel, pixelToColor)
import Example.Instance.Tiles.Plane as TilesPlane
import Example.Render exposing (Renderer)
import Maybe


type alias Model =
    { status : Status
    , options : Solver.Options
    , example : CurrentExample
    , images : Dict ImageAlias Image
    , tiles : Dict TileGroup ( TileSet, Adjacency )
    , patterns : Maybe UniquePatterns
    , matches : Maybe (Neighbours (Matches Int))
    , imagesErrors : Dict ImageAlias Http.Error
    , tilesErrors : Dict TileGroup String
    , workerError : Maybe String
    }


type Status
    = None
    | WaitingRunResponse
    | WaitingTracingResponse
    | WaitingPatternsResponse Status
    | WaitingMatchesResponse Status
    | Tracing


type CurrentExample
    = NotSelected
    | Textual ( Vec2, String ) (Maybe (Grid Char))
    | FromImage Image (Maybe Image)
    | FromTiles TileGroup TileMapping (Maybe (Grid (TileKey, Rotation)))


type alias Grid a = Array (Array (Array a))


type alias ImageAlias = String


type alias TileGroup = String


type Msg
    = NoOp
    | WorkerError String
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
    | Preprocess
    | FindMatchesAt Vec2
    | ShowMatchesFor PatternId
    -- receiving from Http requests
    | GotImage ImageAlias Image
    | GotTiles TileGroup ( TileSet, Adjacency )
    | ImageLoadError ImageAlias Http.Error
    | TilesLoadError TileGroup String
    -- receiving from worker
    | GotResult (Grid Int)
    | GotPatterns UniquePatterns
    | GotMatches Vec2 (Neighbours (Matches Int))
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
    [ "Kotlin"
    , "Castle"
    , "Circles"
    , "Circuit"
    , "Knots"
    , "Rooms"
    , "Summer"
    ]


init : Model
init =
    { status = None
    , options = defaultOptions
    , example = NotSelected
    , images = Dict.empty
    , tiles = Dict.empty
    , patterns = Nothing
    , matches = Nothing
    , workerError = Nothing
    , imagesErrors = Dict.empty
    , tilesErrors = Dict.empty
    }


defaultOptions =
    { approach =
        Overlapping
            { inputBoundary = Bounded -- Periodic
            , patternSize = ( 2, 2 )
            , symmetry = FlipAndRotate
            }
    , outputSize = ( 10, 10 )
    , outputBoundary = Bounded
    }


forText = changeSymmetry FlipAndRotate


forImage = changeSymmetry FlipAndRotate


forTiles = changeSymmetry NoSymmetry


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

                Textual source _  ->
                    (
                        { model
                        | matches = Nothing
                        , status = WaitingRunResponse
                        }
                    , runInWorker
                        { options =
                            Options.encode model.options
                        , source =
                            source
                                |> TextPlane.boundedStringToGrid
                                |> List.map (List.map Char.toCode)
                                |> List.map Array.fromList
                                |> Array.fromList
                        }
                    )

                FromImage source _ ->
                    (
                        { model
                        | matches = Nothing
                        , status = WaitingRunResponse
                        }
                    , runInWorker
                        { options =
                            Options.encode model.options
                        , source =
                            source
                                |> ImageC.toArray2d
                                |> Array.map (Array.map colorToPixel)
                        }
                    )

                FromTiles tileGroup mapping _ ->
                    (
                        { model
                        | matches = Nothing
                        , status = WaitingRunResponse
                        }
                    , case model.tiles |> Dict.get tileGroup of
                        Just (_, FromGrid grid) ->
                            runInWorker
                                { options =
                                    Options.encode model.options
                                , source =
                                    grid
                                        |> Array.map (Array.map <| toIndexInSet mapping)
                                }
                        _ -> Cmd.none
                    )

        Trace ->
            case model.example of

                NotSelected ->
                    ( model, Cmd.none )

                Textual source _ ->
                    (
                        { model
                        | matches = Nothing
                        , status = WaitingTracingResponse
                        }
                    ,
                        traceInWorker
                            { options =
                                Options.encode model.options
                            , source =
                                source
                                    |> TextPlane.boundedStringToGrid
                                    |> List.map (List.map Char.toCode)
                                    |> List.map Array.fromList
                                    |> Array.fromList
                            }
                    )

                FromImage source _ ->
                    (
                        { model
                        | matches = Nothing
                        , status = WaitingTracingResponse
                        }
                    , traceInWorker
                        { options =
                            Options.encode model.options
                        , source =
                            source
                                |> ImageC.toArray2d
                                |> Array.map (Array.map colorToPixel)
                        }
                    )

                FromTiles tileGroup mapping _ ->
                    (
                        { model
                        | matches = Nothing
                        , status = WaitingTracingResponse
                        }
                    , case model.tiles |> Dict.get tileGroup of
                        Just (_, FromGrid grid) ->
                            runInWorker
                                { options =
                                    Options.encode model.options
                                , source =
                                    grid
                                        |> Array.map (Array.map <| toIndexInSet mapping)
                                }
                        _ -> Cmd.none
                    )

        Step ->
            (
                { model
                | matches = Nothing
                , status = WaitingTracingResponse
                }
            , stepInWorker ()
            )

        StepBack ->
            (
                { model
                | matches = Nothing
                , status = WaitingTracingResponse
                }
            , stepBackInWorker ()
            )

        Stop ->
            (
                { model
                | matches = Nothing
                , status = None
                , example =
                    case model.example of
                        NotSelected ->
                            NotSelected
                        Textual source _ ->
                            Textual source Nothing
                        FromImage image _ ->
                            FromImage image Nothing
                        FromTiles group mapping _ ->
                            FromTiles group mapping Nothing
                }
            , stopWorker ()
            )

        Preprocess ->
            case model.example of

                NotSelected ->
                    ( model, Cmd.none )

                Textual source _  ->
                    (
                        { model
                        | status =
                            WaitingPatternsResponse model.status
                        }
                    , preprocessInWorker
                        { options =
                            Options.encode model.options
                        , source =
                            source
                                |> TextPlane.boundedStringToGrid
                                |> List.map (List.map Char.toCode)
                                |> List.map Array.fromList
                                |> Array.fromList
                        }
                    )

                FromImage source _ ->
                    (
                        { model
                        | status =
                            WaitingPatternsResponse model.status
                        }
                    , preprocessInWorker
                        { options =
                            Options.encode model.options
                        , source =
                            source
                                |> ImageC.toArray2d
                                |> Array.map (Array.map colorToPixel)
                        }
                    )

                FromTiles tileGroup mapping _ ->
                    (
                        { model
                        | status =
                            WaitingPatternsResponse model.status
                        }
                    , case model.tiles |> Dict.get tileGroup of
                        Just (_, FromGrid grid) ->
                            preprocessInWorker
                                { options =
                                    Options.encode model.options
                                , source =
                                    grid
                                        |> Array.map (Array.map <| toIndexInSet mapping)
                                }
                        _ -> Cmd.none
                    )

        FindMatchesAt pos ->
            (
                { model
                | matches = Nothing
                , status = WaitingMatchesResponse model.status
                }
            , getMatchesAt pos
            )

        ShowMatchesFor patternId ->
            (
                { model
                | matches =
                    model.patterns
                        |> Maybe.andThen (Dict.get patternId)
                        |> Maybe.map (.matches >> Dict.toList)
                        |> Maybe.map
                            (List.map (Tuple.mapBoth Neighbours.toDirection Matches.fromList))
                        |> Maybe.map Neighbours.fromList
                        |> Maybe.map (Neighbours.map (Maybe.withDefault Matches.none))
                }
            , Cmd.none
            )

        SwitchToTextExample source ->
            (
                { model
                | status = None
                , options = forText model.options
                , example = Textual source Nothing
                , patterns = Nothing
                , matches = Nothing
                }
            , Cmd.none
            )

        SwitchToImageExample image ->
            (
                { model
                | status = None
                , options = forImage model.options
                , example = FromImage image Nothing
                , patterns = Nothing
                , matches = Nothing
                }
            , Cmd.none
            )

        SwitchToTiledExample group ->
            (
                { model
                | status = None
                , options = forTiles model.options
                , example =
                    FromTiles
                        group
                        (case model.tiles |> Dict.get group of
                            Just (tileSet, _) ->
                                buildMapping tileSet
                            _ ->
                                noMapping
                        )
                        Nothing
                , patterns = Nothing
                , matches = Nothing
                }
            , Cmd.none
            )

        ChangeN n ->
            (
                { model
                | options =
                    model.options |> changeN n
                }
            , Cmd.none
            )

        ChangeSymmetry symmetry ->
            (
                { model
                | options =
                    model.options |> changeSymmetry symmetry
                }
            , Cmd.none
            )

        ChangeOutputWidth w ->
            (
                { model
                | options =
                    model.options
                        |> (changeOutputSize <| \(_, h) -> (w, h))
                }
            , Cmd.none
            )

        ChangeOutputHeight h ->
            (
                { model
                | options =
                    model.options
                        |> (changeOutputSize <| \(w, _) -> (w, h))
                }
            , Cmd.none
            )

        UsePeriodicInput ->
            (
                { model
                | options =
                    model.options
                        |> changeInputBoundary Periodic
                }
            , Cmd.none
            )

        UseBoundedInput ->
            (
                { model
                | options =
                    model.options
                        |> changeInputBoundary Bounded
                }
            , Cmd.none
            )

        UsePeriodicOutput ->
            (
                { model
                | options =
                    model.options
                        |> changeOutputBoundary Periodic
                }
            , Cmd.none
            )

        UseBoundedOutput ->
            (
                { model
                | options =
                    model.options
                        |> changeOutputBoundary Bounded
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

        GotTiles tileGroup tiles ->
            (
                { model
                | tiles =
                    model.tiles
                        |> Dict.insert tileGroup tiles
                }
            , Cmd.none
            )

        ImageLoadError imageAlias error ->
            (
                { model
                | imagesErrors =
                    model.imagesErrors
                        |> Dict.insert imageAlias error
                }
            , Cmd.none
            )

        TilesLoadError tileGroup error ->
            (
                { model
                | tilesErrors =
                    model.tilesErrors
                        |> Dict.insert tileGroup error
                }
            , Cmd.none
            )

        GotResult grid ->
            (
                { model
                | status =
                    case model.status of
                        None -> None
                        WaitingRunResponse -> None
                        WaitingTracingResponse -> Tracing
                        WaitingPatternsResponse prevStatus -> prevStatus
                        WaitingMatchesResponse prevStatus -> prevStatus
                        Tracing -> Tracing
                , example
                    = case model.example of

                        NotSelected ->
                            NotSelected

                        Textual source _ ->
                            Textual
                                source
                                (
                                    grid
                                        |> Array.map
                                            (Array.map <| Array.map Char.fromCode)
                                        |> Just
                                )

                        FromImage image _ ->
                            FromImage
                                image
                                (
                                    grid
                                        |> Array.map
                                            (Array.map
                                                <| Array.map pixelToColor
                                                    >> Array.toList
                                                    >> ImagePlane.merge)
                                        |> ImageC.fromArray2d
                                        |> Just

                                )

                        FromTiles group mapping _ ->
                            FromTiles
                                group
                                mapping
                                ( case model.tiles |> Dict.get group of
                                    Just (_, FromGrid _) ->
                                        grid
                                            |> Array.map
                                                (Array.map
                                                    <| Array.map
                                                    <| fromIndexInSet mapping
                                                )
                                            |> Just
                                    _ ->
                                        Nothing  -- FIXME: TODO
                                )

                }
            , Cmd.none
            )

        GotPatterns patterns ->
            (
                { model
                | status =
                    case model.status of
                        WaitingPatternsResponse prevStatus -> prevStatus
                        _ -> None
                , patterns =
                    Just patterns
                }
            , Cmd.none
            )

        GotMatches _ matches ->
            (
                { model
                | status =
                    case model.status of
                        WaitingMatchesResponse prevStatus -> prevStatus
                        _ -> None
                , matches =
                    Just matches
                }
            , Cmd.none
            )

        WorkerError error -> -- TODO
            (
                { model
                | workerError = Just error
                }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let

        viewExample =
            case model.example of

                Textual source wave ->
                    div
                        []
                        [ viewSource TextRenderer.make source
                        , hr [] []
                        , viewNeighboursLoadingArea ( 25, 25 )
                            <| Maybe.withDefault (0, 0)
                            <| Maybe.andThen loadSize
                            <| wave
                        , wave
                            |> Maybe.map
                                (viewGrid <| Array.toList >> TextPlane.merge >> TextRenderer.char)
                            |> Maybe.withDefault (div [] [])
                        ]

                FromImage source wave ->
                    div
                        []
                        [ viewSource ImageRenderer.make source
                        , hr [] []
                        , viewNeighboursLoadingArea ( 10, 10 )
                            <| Maybe.withDefault (0, 0)
                            <| Maybe.map (\{ width, height } -> ( width, height ))
                            <| Maybe.map Image.dimensions
                            <| wave
                        , wave
                            |> Maybe.map ImageRenderer.drawImage
                            |> Maybe.withDefault (div [] [])
                        ]
                    --Example.view ImageRenderer.make exampleModel

                FromTiles group _ wave ->
                    case model.tiles |> Dict.get group of
                        Just ( ( format, tiles ), adjacency ) ->
                            div []
                                [ div
                                    []
                                    <| List.map TilesRenderer.tile
                                    <| List.map (toTileUrl format group)
                                    <| List.map (\key -> ( key, 0 ))  -- FIXME
                                    <| List.map .key
                                    <| tiles
                                , hr [] []
                                , viewNeighboursLoadingArea ( 10, 10 ) -- FIXME: size of a tile
                                    <| Maybe.withDefault (0, 0)
                                    <| Maybe.andThen loadSize
                                    <| wave
                                , case adjacency of
                                    FromGrid grid ->
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

        viewPattern patternId pattern =
            case model.example of
                Textual _ _ ->
                    (TextRenderer.make |> Tuple.second) <| Plane.map Char.fromCode <| pattern
                FromImage _ _ ->
                    (ImageRenderer.make |> Tuple.second)
                        <| Plane.map ImagePlane.pixelToColor <| pattern
                FromTiles group mapping _ ->
                    case model.tiles |> Dict.get group of
                        Just ( ( format, _ ), _ ) ->
                            (TilesRenderer.make (toTileUrl format group) |> Tuple.second)
                                <| Plane.map (fromIndexInSet mapping) <| pattern
                        Nothing -> div [] []
                _ -> div [] []

        viewPatterns patterns =
            div
                [ style "display" "flex"
                , style "overflow" "scroll"
                ]
                <| List.map
                    (\(patternId, { pattern } ) ->
                        div
                            [ style "transform" "scale(0.5)"
                            , style "margin" "5px"
                            , style "cursor" "pointer"
                            , style "padding" "3px"
                            , style "border" "1px solid lightgray"
                            , style "border-radius" "3px"
                            , onClick <| ShowMatchesFor patternId
                            ]
                            [ viewPattern patternId pattern ]
                    )
                <| Dict.toList
                <| patterns

        viewMatches neighbours =
            [ [ Dir.NW, Dir.N, Dir.NE ]
            , [ Dir.W,  Dir.X, Dir.E  ]
            , [ Dir.SW, Dir.S, Dir.SE ]
            ]
            |> List.map
                (\directionsRow ->
                    div
                        [ style "display" "flex", style "flex-direction" "row"
                        , style "margin" "20px 20px"
                        ]
                        <|
                            List.map (\dir ->
                                div []
                                    [ text <| Neighbours.dirToString dir
                                    , text <| Vec2.toString <| Neighbours.offsetFor dir
                                    , div
                                            [ style "display" "flex"
                                            , style "flex-direction" "column"
                                            , style "margin" "20px 20px"
                                            ]
                                            <|
                                                (\list -> case list of
                                                    [] -> [ text "NONE" ]
                                                    _ -> list
                                                )
                                            <| List.map
                                                (\patternId ->
                                                case model.patterns |> Maybe.andThen (Dict.get patternId) of
                                                        Just { pattern } ->
                                                            div
                                                                [ style "transform" "scale(0.8)"
                                                                , style "margin" "0px 10px"
                                                                , style "padding" "3px"
                                                                , style "border" "1px solid lightgray"
                                                                , style "border-radius" "3px"
                                                                ]
                                                                [ viewPattern patternId pattern
                                                                ]
                                                        Nothing ->
                                                            div [] []
                                                )
                                            <| Matches.toList
                                            <| Neighbours.get dir neighbours
                                    ]
                            ) directionsRow
                )
            |> div
                [ style "display" "flex"
                , style "flex-direction" "column"
                , style "margin" "10px 0"
                ]

        viewNeighboursLoadingArea itemSize areaSize
            = case model.patterns of
                Just _ ->
                    viewClickableArea areaSize itemSize FindMatchesAt
                Nothing -> div [] []

        viewClickableArea ( width, height ) ( itemWidth, itemHeight ) toMsg =
            List.range 0 (height - 1)
                |> List.map
                    (\row ->
                            List.range 0 (width - 1)
                                |> List.map (Tuple.pair row)
                    )
                |> List.map
                    (\row ->
                        div
                            [ style "display" "flex", style "flex-direction" "row"
                            -- , style "width" <| String.fromInt itemWidth
                            , style "height" <| String.fromInt itemHeight
                            ]
                            <|
                                List.map (\(y, x) ->
                                    div
                                        [ style "width" <| String.fromInt itemWidth ++ "px"
                                        , style "height" <| String.fromInt itemHeight ++ "px"
                                        , style "cursor" "pointer"
                                        , onClick <| toMsg (x, y)
                                        ]
                                        []
                                ) row
                    )
                |> div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "position" "absolute"
                    , style "z-index" "1111"
                    ]

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
                        , controlPanel ""
                            [ input
                                [ type_ "button"
                                , onClick Preprocess
                                , value "Show Patterns"
                                , disabled <| model.status /= None
                                ]
                                [ text "Show Patterns"
                                ]
                            ]
                        , controlPanel "" -- "Run/Trace"
                            [ fancyButton
                                (model.status == None)
                                "▶️"
                                Run
                            , fancyButton
                                (model.status == Tracing || model.status == None)
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

        toTileUrl format group ( tile, _ ) = -- FIXME: use rotation
            "http://localhost:3000/tiled/" ++ group ++ "/" ++ tile ++ "." ++ format

        pixelatedExample imgAlias =
            case ( model.images |> Dict.get imgAlias, model.imagesErrors |> Dict.get imgAlias ) of
                ( Just image, _ ) ->
                    exampleFrame (SwitchToImageExample image)
                        [ img
                            [ src <| Image.toPngUrl image
                            , style "min-width" "50px"
                            , style "min-height" "50px"
                            , style "image-rendering" "pixelated"
                            ]
                            []
                        ]
                ( _, Just error ) ->
                    exampleFrame NoOp
                        [ Html.text <| imgAlias ++ ": Error " ++ errorToString error ]
                ( Nothing, Nothing ) ->
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
                    |> List.map
                        (\group ->
                            case model.tiles |> Dict.get group of
                                Just ( ( format, tiles ), _ ) ->
                                    exampleFrame (SwitchToTiledExample group)
                                        [ img
                                            [ src <| "http://localhost:3000/tiled/"
                                                ++ group ++ "/" ++
                                                (tiles
                                                    |> List.head
                                                    |> Maybe.map .key
                                                    |> Maybe.withDefault ""
                                                )
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
                    (pixelatedExamples |> List.map pixelatedExample)
                )
    in
        div
            [ style "font-family" "sans-serif" ]
            [ examples
            , case model.example of
                NotSelected -> div [] []
                _ -> controls model.options
            , case model.patterns of
                Just patterns -> viewPatterns patterns
                Nothing -> div [] []
            , viewExample
            , case model.matches of
                Just matches -> viewMatches matches
                Nothing -> div [] []
            ]


main : Program {} Model Msg
main =
    Browser.application
        { init =
            \_ _ _ ->
                ( init
                , Cmd.batch
                    [ requestAllImages pixelatedExamples
                    , requestAllRules tiledExamples
                    ]
                )
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions =
            \_ ->
                Sub.batch
                    [ gotWorkerResult GotResult
                    , gotPatternsFromWorker
                        (\value ->
                            case JD.decodeValue Patterns.decode value of
                                Err error -> WorkerError <| JD.errorToString error
                                Ok patterns -> GotPatterns patterns
                        )
                    , gotMatchesFromWorker
                        (\value ->
                            case JD.decodeValue Matches.decodeNeighbours value of
                                Err error -> WorkerError <| JD.errorToString error
                                Ok matches -> GotMatches (0, 0) matches -- FIXME: actual position
                        )
                    ]
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
                (runResult (ImageLoadError imageAlias) (GotImage imageAlias))
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
                >> Result.andThen
                    (\xmlString ->
                        Result.map2
                            Tuple.pair
                            ( XD.run Tiles.decode xmlString )
                            ( XD.run Adjacency.decode xmlString )
                    )
                >> runResult (TilesLoadError tileGroup) (GotTiles tileGroup)
                )
        }

runResult : (x -> b) -> (a -> b) -> Result x a -> b
runResult onError onSuccess result =
    case result of
        Ok val -> onSuccess val
        Err err -> onError err


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

        Http.GoodStatus_ _ bytes ->
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


port runInWorker : { options : JE.Value, source : Array (Array Int) } -> Cmd msg

port traceInWorker : { options : JE.Value, source : Array (Array Int) } -> Cmd msg

port stepInWorker : () -> Cmd msg

port stepBackInWorker : () -> Cmd msg

port stopWorker : () -> Cmd msg

port preprocessInWorker : { options : JE.Value, source : Array (Array Int) } -> Cmd msg

port getMatchesAt : ( Int, Int ) -> Cmd msg

port gotWorkerResult : (Array (Array (Array Int)) -> msg) -> Sub msg

port gotPatternsFromWorker : (JE.Value -> msg) -> Sub msg

port gotMatchesFromWorker : (JE.Value -> msg) -> Sub msg
