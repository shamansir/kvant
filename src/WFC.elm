module WFC exposing
    ( foo )


import Array exposing (Array)


type PatternExtraction
    = Tiles
    | Overlapping


type alias Configuration =
    { extraction: PatternExtraction
    }


type alias Color = { r: Float, g: Float, b : Float }


type alias PixelGrid = Array (Array Color)


type alias TileId = Int


type Tile = Tile TileId PixelGrid


type GeneratedImage = GeneratedImage (Array (Array Tile))


type PreGeneratedImage = PreGeneratedImage PixelGrid (Array (Array (Maybe Tile)))


type Sample = Sample PixelGrid


type Propagator = Propagator (Array (Array (Array TileId)))


type Observation = Observation Wave (Array TileId)


type Wave = Wave (Array (Array Bool))


type Step
    = Conflicting PreGeneratedImage
    | InProgress PreGeneratedImage
    | Generated GeneratedImage


findTiles : Sample -> Array Tile
findTiles _ = Array.empty


initialWave : Wave
initialWave = (Wave Array.empty)


buildPropagator : PatternExtraction -> Array Tile -> Propagator
buildPropagator _ _ = Propagator Array.empty


firstImage : PreGeneratedImage
firstImage = PreGeneratedImage Array.empty Array.empty


observe : PreGeneratedImage -> Observation
observe _ = Observation initialWave (Array.empty)


propagate : Propagator -> Observation -> Step
propagate _ _ = Generated (GeneratedImage Array.empty)


run : Configuration -> Result PreGeneratedImage GeneratedImage
run { extraction } =
    let
        patterns = findTiles <| Sample Array.empty
        propagator = buildPropagator extraction patterns
        observeAndPropagate preImage =
            case observe preImage |> propagate propagator of
                Conflicting conflicting -> Err conflicting
                InProgress inProgress -> observeAndPropagate inProgress
                Generated generatedImage -> Ok generatedImage
    in
        observeAndPropagate firstImage


foo = 42
