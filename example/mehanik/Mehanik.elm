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
import Color exposing (Color)
import Either exposing (Either(..))


import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as H
import Html.Events exposing (..)


import Kvant.Vec2 exposing (Vec2, loadSize)
import Kvant.Vec2 as Vec2
import Kvant.Plane as Plane
import Kvant.Plane as Plane exposing (Boundary(..), Symmetry(..))
import Kvant.Adjacency as Adjacency
import Kvant.Solver.Options as Options
import Kvant.Json.Options as Options
import Kvant.Json.Adjacency as Adjacency
import Kvant.Solver exposing (Adjacency)
import Kvant.Patterns exposing (UniquePatterns)
import Kvant.Json.Patterns as Patterns
import Kvant.Neighbours exposing (Neighbours)
import Kvant.Neighbours as Neighbours
import Kvant.Direction as Dir exposing (Direction(..))
import Kvant.Matches exposing (Matches)
import Kvant.Matches as Matches
import Kvant.Json.Matches as Matches
import Kvant.Patterns exposing (PatternId, Pattern)
import Kvant.Rotation as Rotation exposing (Rotation)
import Kvant.Tiles exposing
    ( toIndexInSet, fromIndexInSet, buildMapping, noMapping
    , TileMapping, TileKey, TileSet, TileGrid, Rule
    , TileAdjacency
    )
import Kvant.Tiles as Tiles
import Kvant.Xml.Tiles as Tiles
import Kvant.Xml.Adjacency as Adjacency

import Mehanik.Server as Server
import Mehanik.Chars as Chars
import Mehanik.Chars as Text
import Mehanik.Colors as Colors
import Mehanik.Colors as Image
--import Mehanik.Patterns as Patterns
import Mehanik.Tiles as Tiles
import Mehanik.Grid as Grid
import Mehanik.Generic exposing (..)
import Mehanik.Patterns as Patterns


-- import Mehanik.Renderer exposing (Renderer)
-- import Mehanik.Grid exposing (applyWave)
import Maybe
import Task

import Mehanik.Controls exposing (..)


type alias Options =  ( Options.PatternSearch, Options.Output )


type alias AtomId = Int -- tile, character, pattern, color, etc...


type alias Model =
    { status : Status
    , options : Options
    , example : CurrentExample
    , images : Dict ImageAlias Image
    , tiles : Dict TileSetName ( TileSet, Either (List Rule) TileGrid )
    , matches : Maybe (Neighbours (Matches AtomId))
    , imagesErrors : Dict ImageAlias Http.Error
    , tilesErrors : Dict TileSetName String
    , workerError : Maybe String
    }


type Status
    = None
    | WaitingRunResponse
    | WaitingTracingResponse
    | WaitingAdjacencyResponse Status
    | WaitingMatchesResponse Status
    | Tracing


type CurrentExample
    = NotSelected
    | Textual
        { source : ( Vec2, String )
        , adjacency : Maybe (Adjacency Pattern)
        , wave : Maybe (Grid Char)
        }
    | FromImage
        { source : Image
        , adjacency : Maybe (Adjacency Pattern)
        , wave : Maybe Image
        }
    | FromTiles
        { set : TileSetName
        , mapping : TileMapping
        , adjacency : Maybe
            (Either
                TileAdjacency
                (Adjacency Pattern)
            )
        , wave : Maybe (Grid (TileKey, Rotation))
        }


type alias Grid a = Array (Array (Array a))


type alias ImageAlias = String


type alias TileSetName = String


type Msg
    = NoOp
    | WorkerError String
    -- switch to example
    | SwitchToTextExample ( Vec2, String )
    | SwitchToImageExample Image
    | SwitchToTiledExample TileSetName
    -- controls for worker
    | Run
    | Trace
    | Step
    | StepBack
    | Stop
    | Preprocess
    | FindMatchesAt Vec2
    | ShowMatchesFor AtomId
    -- receiving from Http requests
    | GotImage ImageAlias Image
    | GotTiles TileSetName ( TileSet, Either (List Rule) TileGrid )
    | GotRemoteTileSetNames (List TileSetName)
    | ImageLoadError ImageAlias Http.Error
    | TilesLoadError TileSetName String
    -- receiving from worker
    | GotResult (Grid AtomId)
    | GotAdjacency (Adjacency Pattern)
    | GotMatches Vec2 (Neighbours (Matches AtomId))
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
    , "TriangleAndStick"
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
    , options =
        ( Options.defaultPatternSearch, Options.defaultOutput )
    , example = NotSelected
    , images = Dict.empty
    , tiles = Dict.empty
    , matches = Nothing
    , workerError = Nothing
    , imagesErrors = Dict.empty
    , tilesErrors = Dict.empty
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

                Textual { adjacency }  ->
                    case adjacency of
                        Just theAdjacency ->

                            (
                                { model
                                | matches = Nothing
                                , status = WaitingRunResponse
                                }
                            , runInWorker
                                { options =
                                    model.options
                                        |> Tuple.second
                                        |> Options.encodeOutput
                                 , adjacency =
                                    theAdjacency
                                        |> encodePatternAdjacencyForPort
                                }
                            )

                        Nothing -> ( model, Cmd.none )

                FromImage { adjacency } ->
                    case adjacency of
                        Just theAdjacency ->

                            (
                                { model
                                | matches = Nothing
                                , status = WaitingRunResponse
                                }
                            , runInWorker
                                { options =
                                    model.options
                                        |> Tuple.second
                                        |> Options.encodeOutput
                                 , adjacency =
                                    theAdjacency
                                        |> encodePatternAdjacencyForPort
                                }
                            )
                        Nothing -> ( model, Cmd.none )

                FromTiles { mapping, adjacency } ->
                    case adjacency of
                        Just theAdjacency ->

                            (
                                { model
                                | matches = Nothing
                                , status = WaitingRunResponse
                                }
                            , runInWorker
                                    { options =
                                        model.options
                                            |> Tuple.second
                                            |> Options.encodeOutput
                                    , adjacency =
                                        theAdjacency
                                            |> Either.unpack
                                                (encodeTiledAdjacencyForPort mapping)
                                                encodePatternAdjacencyForPort
                                    }
                            )

                        Nothing -> ( model, Cmd.none )

        Trace ->
            case model.example of

                NotSelected ->
                    ( model, Cmd.none )

                Textual { adjacency } ->
                    case adjacency of
                        Just theAdjacency ->
                            (
                                { model
                                | matches = Nothing
                                , status = WaitingTracingResponse
                                }
                            , traceInWorker
                                { options =
                                    model.options
                                        |> Tuple.second
                                        |> Options.encodeOutput
                                 , adjacency =
                                    theAdjacency
                                        |> encodePatternAdjacencyForPort
                                }
                            )
                        Nothing -> ( model, Cmd.none )

                FromImage { adjacency } ->
                    case adjacency of
                        Just theAdjacency ->

                            (
                                { model
                                | matches = Nothing
                                , status = WaitingTracingResponse
                                }
                            , traceInWorker
                                { options =
                                    model.options
                                        |> Tuple.second
                                        |> Options.encodeOutput
                                 , adjacency =
                                    theAdjacency
                                        |> encodePatternAdjacencyForPort
                                }
                            )

                        Nothing -> ( model, Cmd.none )

                FromTiles { adjacency, mapping } ->

                    case adjacency of
                        Just theAdjacency ->

                            (
                                { model
                                | matches = Nothing
                                , status = WaitingRunResponse
                                }
                            , runInWorker
                                    { options =
                                        model.options
                                            |> Tuple.second
                                            |> Options.encodeOutput
                                    , adjacency =
                                        theAdjacency |>
                                            Either.unpack
                                                (encodeTiledAdjacencyForPort mapping)
                                                encodePatternAdjacencyForPort
                                    }
                            )

                        Nothing -> ( model, Cmd.none )

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
                        Textual spec ->
                            Textual { spec | wave = Nothing }
                        FromImage spec ->
                            FromImage { spec | wave = Nothing }
                        FromTiles spec ->
                            FromTiles { spec | wave = Nothing }
                }
            , stopWorker ()
            )

        Preprocess ->
            case model.example of

                NotSelected ->
                    ( model, Cmd.none )

                Textual { source }  ->
                    (
                        { model
                        | status =
                            WaitingAdjacencyResponse model.status
                        }
                    , preprocessInWorker
                        { options =
                            model.options
                                |> Tuple.first
                                |> Options.encodePatternSearch
                        , source =
                            source
                                |> Chars.boundedStringToGrid
                                |> List.map (List.map Char.toCode)
                                |> List.map Array.fromList
                                |> Array.fromList
                        }
                    )

                FromImage { source } ->
                    (
                        { model
                        | status =
                            WaitingAdjacencyResponse model.status
                        }
                    , preprocessInWorker
                        { options =
                            model.options
                                |> Tuple.first
                                |> Options.encodePatternSearch
                        , source =
                            source
                                |> ImageC.toArray2d
                                |> Array.map (Array.map Colors.colorToPixel)
                        }
                    )

                FromTiles { set, mapping } ->
                    (
                        { model
                        | status =
                            WaitingAdjacencyResponse model.status
                        }
                    , case model.tiles |> Dict.get set of
                        Just (_, Right grid) ->
                            preprocessInWorker
                                { options =
                                    model.options
                                        |> Tuple.first
                                        |> Options.encodePatternSearch
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

        ShowMatchesFor atomId ->
            (
                { model
                | matches =
                    let
                        fromAdjacency adjacency =
                            adjacency
                                |> Dict.get atomId
                                |> Maybe.map (.matches >> Dict.toList)
                                |> Maybe.map (List.map (Tuple.mapFirst Dir.fromOffset))
                                |> Maybe.map Neighbours.fromList
                                |> Maybe.map (Neighbours.map <| Maybe.withDefault Matches.none)
                                |> Maybe.map (Neighbours.set Dir.X <| Matches.single atomId)
                    in
                        case model.example of
                            FromTiles spec ->
                                case spec.adjacency of
                                    Just (Right patternAdjacency)
                                        -> fromAdjacency patternAdjacency
                                    Just (Left tileAdjacency)
                                        -> fromAdjacency
                                                <| Adjacency.mapKey
                                                    (Tuple.mapSecond Rotation.fromId
                                                        >> toIndexInSet spec.mapping)
                                                <| Adjacency.map (toIndexInSet spec.mapping)
                                                <| tileAdjacency
                                    Nothing ->
                                        model.matches
                            Textual { adjacency } ->
                                adjacency
                                    |> Maybe.map fromAdjacency
                                    |> Maybe.withDefault model.matches
                            FromImage { adjacency } ->
                                adjacency
                                    |> Maybe.map fromAdjacency
                                    |> Maybe.withDefault model.matches
                            NotSelected -> model.matches
                }
            , Cmd.none
            )

        SwitchToTextExample source ->
            (
                { model
                | status = None
                , options = forText model.options
                , example =
                    Textual
                        { source = source
                        , adjacency = Nothing
                        , wave = Nothing
                        }
                , matches = Nothing
                }
            , preprocessInWorker_
            )

        SwitchToImageExample image ->
            (
                { model
                | status = None
                , options = forImage model.options
                , example =
                    FromImage
                        { source = image
                        , adjacency = Nothing
                        , wave = Nothing
                        }
                , matches = Nothing
                }
            , preprocessInWorker_
            )

        SwitchToTiledExample set ->
            (
                { model
                | status = None
                , options = forTiles model.options
                , example =

                    FromTiles

                        { set = set

                        , mapping =
                            case model.tiles |> Dict.get set of
                                Just (tileSet, _) ->
                                    buildMapping tileSet
                                _ ->
                                    noMapping

                        , adjacency =
                            case model.tiles |> Dict.get set of
                                Just (_, Right _) ->
                                    Nothing -- will be received from worker
                                Just ( (_, tiles ), Left rules ) ->
                                    Just
                                        <| Left
                                        <| Tiles.buildAdjacencyRules
                                            tiles
                                            rules
                                Nothing ->
                                    Nothing

                        , wave = Nothing
                        }

                , matches = Nothing
                }

            , case model.tiles |> Dict.get set of
                Just (_, Right _) ->
                    preprocessInWorker_
                _ ->
                    Cmd.none
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

        GotTiles tileSet tiles ->
            (
                { model
                | tiles =
                    model.tiles
                        |> Dict.insert tileSet tiles
                }
            , Cmd.none
            )

        GotRemoteTileSetNames tileSetsNames ->
            ( model
            , requestAllRemoteRules
                <| List.filter
                    (\set -> not <| List.member set tiledExamples)
                <| tileSetsNames
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

        TilesLoadError tileSet error ->
            (
                { model
                | tilesErrors =
                    model.tilesErrors
                        |> Dict.insert tileSet error
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
                        WaitingAdjacencyResponse prevStatus -> prevStatus
                        WaitingMatchesResponse prevStatus -> prevStatus
                        Tracing -> Tracing

                , example =

                    case model.example of

                        NotSelected ->
                            NotSelected

                        Textual spec ->
                            Textual
                                { spec
                                | wave =
                                    spec.adjacency
                                        |> Maybe.map
                                            (\adjacency ->

                                                Patterns.applyWave
                                                    -1
                                                    (Array.map Char.fromCode)
                                                    grid
                                                    adjacency

                                            )
                                }


                        FromImage spec ->
                            FromImage
                                { spec
                                | wave =
                                    spec.adjacency
                                        |> Maybe.map
                                            (\adjacency ->

                                                Patterns.applyWave
                                                    -1
                                                    (Array.map Colors.pixelToColor
                                                                >> Array.toList
                                                                >> Colors.merge)
                                                    grid
                                                    adjacency
                                                    |> ImageC.fromArray2d

                                            )
                                }

                        FromTiles spec ->
                            FromTiles
                                { spec
                                | wave =
                                    ( case model.tiles |> Dict.get spec.set of
                                        Just _ ->
                                            case spec.adjacency of

                                                Just (Right adjacency) ->
                                                                                                                                                        Patterns.applyWave
                                                        -1
                                                        (Array.map <| fromIndexInSet spec.mapping)
                                                        grid
                                                        adjacency
                                                        |> Just

                                                Just (Left _) ->

                                                    grid
                                                        |> Grid.adaptGrid
                                                            (Array.map
                                                            <| fromIndexInSet spec.mapping)
                                                        |> Just

                                                Nothing ->
                                                    Nothing

                                        _ ->
                                            Nothing  -- FIXME: TODO
                                    )
                                }

                }
            , Cmd.none
            )

        GotAdjacency adjacency ->
            (
                { model
                | status =
                    case model.status of
                        WaitingAdjacencyResponse prevStatus -> prevStatus
                        _ -> None
                , example = model.example |> setPatternAdjacency adjacency
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

                Textual { source, wave } ->
                    div
                        []
                        [ Text.renderInput source
                        , hr [] []
                        , viewNeighboursLoadingArea ( 25, 25 )
                            <| Maybe.withDefault (0, 0)
                            <| Maybe.andThen loadSize
                            <| wave
                        , wave
                            |> Maybe.map
                                (Grid.grid1 <| Array.toList >> Chars.merge >> Chars.char)
                            |> Maybe.withDefault (div [] [])
                        ]

                FromImage { source, wave } ->
                    div
                        []
                        [ Image.renderInput source
                        , hr [] []
                        , viewNeighboursLoadingArea ( 10, 10 )
                            <| Maybe.withDefault (0, 0)
                            <| Maybe.map (\{ width, height } -> ( width, height ))
                            <| Maybe.map Image.dimensions
                            <| wave
                        , wave
                            |> Maybe.map Colors.drawImage
                            |> Maybe.withDefault (div [] [])
                        ]
                    --Example.view ImageRenderer.make exampleModel

                FromTiles { set, wave, mapping } ->
                    case model.tiles |> Dict.get set of
                        Just ( ( format, tiles ), maybeGrid ) ->
                            div []
                                [ hr [] []
                                , Tiles.viewTiles toTileUrl ShowMatchesFor mapping format set
                                    <| case currentTiles of
                                            Just adjacencyTiles ->
                                                Dict.keys adjacencyTiles
                                            Nothing ->
                                                tiles
                                                    |> List.map .key
                                                    |> List.map (\key -> ( key, 0 ))  -- FIXME
                                , hr [] []
                                , viewNeighboursLoadingArea ( 10, 10 ) -- FIXME: size of a tile
                                    <| Maybe.withDefault (0, 0)
                                    <| Maybe.andThen loadSize
                                    <| wave
                                , div
                                    [ style "transform" "scale(0.6)"
                                    , style "transform-origin" "0 0"
                                    ]
                                    [ case maybeGrid of
                                        Right grid ->
                                            Tiles.renderInput
                                                (toTileUrl format set)
                                                grid
                                        Left _ ->
                                            div [] []  -- FIXME: TODO
                                    , wave
                                        |> Maybe.map
                                            (Grid.grid1
                                                <| Array.toList
                                                    >> Tiles.merge
                                                    >> Tiles.tileAndCount
                                                            (toTileUrl format set)
                                            )
                                        |> Maybe.withDefault (div [] [])
                                    ]
                                ]

                        Nothing -> div [] []
                NotSelected -> Html.text "Not Selected"
                -- WaitingForImage url -> Html.text <| "Waiting for image " ++ url ++ " to load"

        viewPattern patternId pattern =
            case model.example of
                Textual _ ->
                    -- FIXME:
                    Text.renderPlane <| Plane.map Char.fromCode <| pattern
                FromImage _ ->
                    -- FIXME:
                    Image.renderPlane
                        <| Plane.map Image.pixelToColor <| pattern
                FromTiles { set, mapping } ->
                    case model.tiles |> Dict.get set of
                        Just ( ( format, _ ), _ ) ->
                            Tiles.renderPlane (toTileUrl format set)
                                <| Plane.map (fromIndexInSet mapping) <| pattern
                        Nothing -> div [] []
                _ -> div [] []

        currentPatterns = getPatternAdjacency model.example

        currentTiles = getTileAdjacency model.example

        viewMatch =
            case model.example of
                FromTiles spec ->
                    case spec.adjacency of
                        Just (Left _) ->
                            let

                                format =
                                    model.tiles
                                        |> Dict.get spec.set
                                        |> Maybe.map (Tuple.first >> Tuple.first)
                                        |> Maybe.withDefault "png"

                            in
                                \matchId ->

                                    case currentTiles
                                        |> Maybe.andThen
                                            (Dict.get
                                                <| Tuple.mapSecond Rotation.toId
                                                <| fromIndexInSet spec.mapping
                                                <| matchId
                                            ) of
                                        Just { subject } ->
                                            Tiles.tile1
                                                (toTileUrl format spec.set)
                                                subject
                                        Nothing -> div [] []

                        Just (Right _) ->

                            \matchId ->
                                case currentPatterns
                                    |> Maybe.andThen (Dict.get matchId) of
                                    Just { subject } ->
                                        viewPattern matchId subject
                                    Nothing -> div [] []

                        Nothing -> always <| div [] []

                _ ->
                    \matchId ->
                        case currentPatterns
                            |> Maybe.andThen (Dict.get matchId) of
                            Just { subject } ->
                                viewPattern matchId subject
                            Nothing -> div [] []


        viewNeighboursLoadingArea itemSize areaSize
            = case currentPatterns of
                Just _ ->
                    viewClickableArea areaSize itemSize FindMatchesAt
                Nothing -> div [] []


        controls (search, ( outputBoundary, outputSize ))  =
            div
                []

                [ controlPanel "Pattern size"
                    [ checkbox
                        (case search.patternSize of (n, _) -> n == 2)
                        (model.status /= None)
                        "2x"
                        <| ChangeN (2, 2)
                    , checkbox
                        (case search.patternSize of (n, _) -> n == 3)
                        (model.status /= None)
                        "3x"
                        <| ChangeN (3, 3)
                    ]

                , controlPanel "Input"
                    [ checkbox
                        (case search.boundary of
                            Periodic -> True
                            Bounded -> False)
                        (model.status /= None)
                        "Periodic"
                        UsePeriodicInput
                    , checkbox
                        (case search.boundary of
                            Periodic -> False
                            Bounded -> True)
                        (model.status /= None)
                        "Bounded"
                        UseBoundedInput
                    ]

                , controlPanel "Symmetry"
                    [ checkbox
                        (case search.symmetry of
                            NoSymmetry -> True
                            _ -> False)
                        (model.status /= None)
                        "None"
                        <| ChangeSymmetry NoSymmetry
                    , checkbox
                        (case search.symmetry of
                            FlipOnly -> True
                            _ -> False)
                        (model.status /= None)
                        "Only flip"
                        <| ChangeSymmetry FlipOnly
                    , checkbox
                        (case search.symmetry of
                            RotateOnly -> True
                            _ -> False)
                        (model.status /= None)
                        "Only rotate"
                        <| ChangeSymmetry RotateOnly
                    , checkbox
                        (case search.symmetry of
                            FlipAndRotate -> True
                            _ -> False)
                        (model.status /= None)
                        "Flip and rotate"
                        <| ChangeSymmetry FlipAndRotate
                    ]

                , controlPanel "Output"
                    [ checkbox
                        (case outputBoundary of
                            Periodic -> True
                            Bounded -> False)
                        (model.status /= None)
                        "Periodic"
                        UsePeriodicOutput
                    , checkbox
                        (case outputBoundary of
                            Periodic -> False
                            Bounded -> True)
                        (model.status /= None)
                        "Bounded"
                        UseBoundedOutput
                    ]

                , controlPanel "Output size"
                    [ span
                        []
                        [ Html.text
                            <| "Width ("
                                ++ (String.fromInt <| Tuple.first <| outputSize)
                                ++ ")"]
                    , input
                        [ type_ "range"
                        , H.min "8"
                        , H.max "96"
                        , value <| String.fromInt <| Tuple.first <| outputSize
                        , onInput
                            <| String.toInt >> Maybe.withDefault 8 >> ChangeOutputWidth
                        ]
                        []
                    , span
                        []
                        [ Html.text
                            <| "Height ("
                                ++ (String.fromInt <| Tuple.second <| outputSize)
                                ++ ")"]
                    , input
                        [ type_ "range"
                        , H.min "8"
                        , H.max "96"
                        , value <| String.fromInt <| Tuple.second <| outputSize
                        , onInput <|
                            String.toInt >> Maybe.withDefault 8 >> ChangeOutputHeight
                        ]
                        []
                    ]

                , case currentPatterns of
                    Just _ ->
                        controlPanel ""
                            [ input
                                [ type_ "button"
                                , onClick Preprocess
                                , value "Show Patterns"
                                , disabled <| model.status /= None
                                ]
                                [ text "Show Patterns"
                                ]
                            ]
                    Nothing -> div [] []

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

        toTileUrl format set ( tile, _ ) = -- FIXME: use rotation
            "http://localhost:3000/tiled/" ++ set ++ "/" ++ tile ++ "." ++ format

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
                        [ Html.text <| imgAlias ++ ": Error " ++ Server.errorToString error ]
                ( Nothing, Nothing ) ->
                    exampleFrame NoOp
                        [ Html.text <| imgAlias ++ ": Loading..." ]


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
                                [ Text.renderInput boundedStr
                                ]
                        )
                    )
                ++
                    (model.tiles
                    |> Dict.keys
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
            , case currentPatterns of
                Just patterns ->
                    Patterns.viewPatterns
                        (Tuple.first >> ShowMatchesFor)
                        (\( patternId, { subject } ) ->
                            viewPattern patternId subject
                        )
                        patterns
                Nothing -> div [] []
            , viewExample
            , case model.matches of
                Just matches -> viewMatches viewMatch matches
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
                    , requestAllLocalRules tiledExamples
                    , Server.requestTilesets
                        (\result ->
                            case result of
                                Ok tileSetsNames -> GotRemoteTileSetNames tileSetsNames
                                Err error -> TilesLoadError "(remote)" <| Server.errorToString error
                        )
                    ]
                )
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        , subscriptions =
            \_ ->
                Sub.batch
                    [ gotWorkerResult GotResult
                    , gotAdjacencyFromWorker
                        (\value ->
                            case JD.decodeValue Patterns.decode value of
                                Err error -> WorkerError <| JD.errorToString error
                                Ok adjacency -> GotAdjacency adjacency
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


requestImage : ImageAlias -> Cmd Msg
requestImage imageAlias =
    Http.get
        { url = "http://localhost:3000/overlapping/" ++ imageAlias ++ ".png"
        , expect =
            Http.expectBytesResponse
                (runResult (ImageLoadError imageAlias) (GotImage imageAlias))
                Server.loadImage
        }


requestAllImages : List ImageAlias -> Cmd Msg
requestAllImages = List.map requestImage >> Cmd.batch


requestLocalRules : TileSetName -> Cmd Msg
requestLocalRules tileSet =
    Http.get
        { url = "http://localhost:3000/tiled/" ++ tileSet ++ "/data.xml"
        , expect = Server.expectTileRules
        }
        |> Cmd.map (runResult (TilesLoadError tileSet) (GotTiles tileSet))


runResult : (x -> b) -> (a -> b) -> Result x a -> b
runResult onError onSuccess result =
    case result of
        Ok val -> onSuccess val
        Err err -> onError err


requestAllLocalRules : List TileSetName -> Cmd Msg
requestAllLocalRules = List.map requestLocalRules >> Cmd.batch


requestRemoteRules : TileSetName -> Cmd Msg
requestRemoteRules tileSet =
    Server.requestRules tileSet <| runResult (TilesLoadError tileSet) (GotTiles tileSet)


requestAllRemoteRules : List TileSetName -> Cmd Msg
requestAllRemoteRules = List.map requestRemoteRules >> Cmd.batch


getPatternAdjacency : CurrentExample -> Maybe (Adjacency Pattern)
getPatternAdjacency example =
    case example of
        NotSelected -> Nothing
        Textual { adjacency } -> adjacency
        FromImage { adjacency } -> adjacency
        FromTiles { adjacency } -> adjacency |> Maybe.andThen (Either.unwrap Nothing Just)


getTileAdjacency : CurrentExample -> Maybe TileAdjacency
getTileAdjacency example =
    case example of
        NotSelected -> Nothing
        Textual _ -> Nothing
        FromImage _ -> Nothing
        FromTiles { adjacency } ->
            adjacency
                |> Maybe.andThen (Either.swap >> Either.unwrap Nothing Just)


setPatternAdjacency : Adjacency Pattern -> CurrentExample -> CurrentExample
setPatternAdjacency adjacency example =
    case example of
        NotSelected -> NotSelected
        Textual spec -> Textual { spec | adjacency = Just adjacency }
        FromImage spec -> FromImage { spec | adjacency = Just adjacency }
        FromTiles spec ->
            FromTiles { spec | adjacency = Just <| Right adjacency }


encodePatternAdjacencyForPort : Adjacency Pattern -> JE.Value
encodePatternAdjacencyForPort =
    Adjacency.reflective >> Adjacency.encode


encodeTiledAdjacencyForPort : TileMapping -> TileAdjacency -> JE.Value
encodeTiledAdjacencyForPort ( _, keyToId ) =
    let
        convert key = keyToId |> Dict.get key |> Maybe.withDefault -1
    in
        Adjacency.mapKey convert
            >> Adjacency.map (Tuple.mapSecond Rotation.toId >> convert)
            >> Adjacency.encode


preprocessInWorker_ : Cmd Msg
preprocessInWorker_ =
    Task.succeed Preprocess
        |> Task.perform identity


changeN : Plane.Size -> Options -> Options
changeN n ( search, output ) =
    (
        { search
        | patternSize = n
        }
    , output
    )


changeSymmetry : Symmetry -> Options -> Options
changeSymmetry symmetry ( search, output ) =
    (
        { search
        | symmetry = symmetry
        }
    , output
    )


changeInputBoundary : Boundary -> Options -> Options
changeInputBoundary boundary ( search, output ) =
    (
        { search
        | boundary = boundary
        }
    , output
    )


changeOutputBoundary : Boundary -> Options -> Options
changeOutputBoundary boundary ( search, ( _, size ) ) =
    ( search, ( boundary, size ) )


changeOutputSize : (Plane.Size -> Plane.Size) -> Options -> Options
changeOutputSize f ( search, ( boundary, size ) ) =
    ( search, ( boundary, f size ) )


port runInWorker : { options : JE.Value, adjacency : JE.Value } -> Cmd msg

port traceInWorker : { options : JE.Value, adjacency : JE.Value } -> Cmd msg

port stepInWorker : () -> Cmd msg

port stepBackInWorker : () -> Cmd msg

port stopWorker : () -> Cmd msg

port preprocessInWorker : { options : JE.Value, source : Array (Array Int) } -> Cmd msg

port getMatchesAt : ( Int, Int ) -> Cmd msg

port gotWorkerResult : (Array (Array (Array Int)) -> msg) -> Sub msg

port gotAdjacencyFromWorker : (JE.Value -> msg) -> Sub msg

port gotMatchesFromWorker : (JE.Value -> msg) -> Sub msg
