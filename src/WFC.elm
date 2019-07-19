module WFC exposing
    ( foo )




import Array exposing (Array)


type PatternExtraction
    = Simple
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


type Propagator = Propagator ()


type Observation = Observation ()


type alias Step = Result PreGeneratedImage GeneratedImage


findTiles : Sample -> Array Tile
findTiles _ = Array.empty


buildPropagator : Array Tile -> Propagator
buildPropagator _ = Propagator ()


firstStep : Step
firstStep = Err (PreGeneratedImage Array.empty Array.empty)


observe : Step -> Observation
observe _ = Observation ()


propagate : Propagator -> Observation -> Step
propagate _ _ = Ok (GeneratedImage Array.empty)



run : Configuration -> Result PreGeneratedImage GeneratedImage
run _ =
    let
        patterns = findTiles <| Sample Array.empty
        propagator = buildPropagator patterns
        observeAndPropagate step =
            case observe step |> propagate propagator of
                Ok image -> Ok image
                Err further -> observeAndPropagate (Err further)

    in
        observeAndPropagate firstStep


foo = 42

