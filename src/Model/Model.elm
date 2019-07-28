module Model
    exposing (Model)


import Array exposing (Array)
import Random


type FMX = FMX Int
type FMY = FMY Int
type T = T Int
type Size = Size FMX FMY
type Limit = Limit Int


type alias CheckBoundary = { x : Int, y : Int } -> Bool


type WaveCell
    = Collapsed
    | Unknown
type Wave = Wave (List (List WaveCell))


type MatchingPatternsCount
    = Zero
    | One
    | MoreThanOne
type Entropies = Entropies (List Float)


type Observation
    = Continue -- null
    | Contradiction -- false
    | Finished -- true


type Graphics = Graphics {}
type UnfinishedGraphics = UnfinishedGraphics {}
type RunResolution
    = GotContradiction
    | ReachedLimit UnfinishedGraphics
    | Success Graphics


type alias Compatibility =
    { a : Int
    , b : Int
    , c : Int
    , d : Int
    }


type alias Weights =
    { values : List Float
    , sum : Float
    , raisedToLog : List Float
    , sumRaisedToLog : Float
    }


type alias Sums =
    { amounts : List MatchingPatternsCount
    , weights : List Float
    , weightsLogged : List Float
    }


type alias Model =
    { size : Size
    , distribution : List Int
    , wave : List (List WaveCell)
    , compatible : List (List Compatibility)
    , weights : Weights
    , sums : Sums
    , entropies : Entropies
    , startingEntropy : Float
    , stack : List Int
    , stackSize : Int
    -- , startingEntropy : Float
    }


type MinEntropy
    = FoundContradiction
    | FoundAt Int
    | NotFound


-- dx = [-1, 0, 1, 0]
-- dy = [0, 1, 0, -1]
-- opposite = [2, 3, 0, 1]


square : Size -> Int
square (Size (FMX fmx) (FMY fmy)) =
    fmx * fmy


init : Size -> T -> Model
init size (T t) =
    let
        count = square size
        weights =
            List.repeat t 0 |> weight
        sums =
            { amounts = List.repeat count Zero
            , weights = List.repeat count 0
            , weightsLogged = List.repeat count 0
            }
    in
        { distribution = List.repeat t 0
        , wave = List.repeat count <| List.repeat t Unknown
        , compatible =
            List.repeat count
                <| List.repeat t
                <| { a = 0, b = 0, c = 0, d = 0 }
        , weights = weights
        , startingEntropy =
            logBase e weights.sum - weights.sumRaisedToLog / weights.sum
        , entropies = List.repeat count 0 |> Entropies
        , sums = sums
        , stack = List.repeat t 0
        , stackSize = 0
        , size = size
        }


weight : List Float -> Weights
weight values =
    let
        raisedToLog = List.map (\w -> w * logBase e w) values
    in
        { values = values
        , sum = List.foldl (+) 0 values
        , raisedToLog = raisedToLog
        , sumRaisedToLog = List.foldl (+) 0 raisedToLog
        }


findMinimumEntropy
    :  Size
    -> CheckBoundary
    -> List MatchingPatternsCount
    -> Entropies
    -> Random.Seed
    -> ( Random.Seed, MinEntropy )
findMinimumEntropy
    (Size (FMX fmx) (FMY fmy))
    onBoundary
    amounts
    (Entropies entropies)
    seed =
    let
        advance ( index, ( amount, entropy ) ) ( lastSeed, prevResult ) =
            case ( amount, prevResult ) of
                ( _, Err err ) -> ( lastSeed, Err err ) -- stop (contradiction happened before)
                ( Zero, _ ) -> ( lastSeed, Err () ) -- stop (zero options -> contradictiom)
                ( One, _ ) -> ( lastSeed, prevResult ) -- continue (amount is one -> entropy is 0)
                ( MoreThanOne, Ok ( lastMin, lastIndexOfMin ) ) ->
                    if (entropy > lastMin)
                       || onBoundary
                            { x = modBy fmx index
                            , y = floor <| toFloat index / toFloat fmx
                            }
                    then ( lastSeed, prevResult ) -- continue
                    else
                        let
                            ( noise, nextSeed ) =
                                Random.step
                                    (Random.float 0 1 |> Random.map ((*) 0.000001))
                                    lastSeed
                        in
                            ( nextSeed
                            , Ok <|
                                if entropy + noise < lastMin
                                    then ( entropy + noise, index )
                                    else ( lastMin, lastIndexOfMin )
                            )
        search =
            List.map2 Tuple.pair amounts entropies
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    advance
                    ( seed
                    , Ok
                        ( Random.minInt |> toFloat
                        , -1
                        )
                    )
    in
        case search of
            ( lastSeed, Ok ( _, indexOfMin ) ) ->
                if ( indexOfMin == -1) then
                    ( lastSeed, NotFound )
                else ( lastSeed, FoundAt indexOfMin )
            ( lastSeed, Err _ )  ->
                ( lastSeed, FoundContradiction )


observe
    :  CheckBoundary
    -> Model
    -> Random.Seed
    -> ( Random.Seed, Observation, Model )
observe checkBoundary model seed =
    let
        ( nextSeed, minimumEntropy ) =
            findMinimumEntropy
                model.size
                checkBoundary
                model.sums.amounts
                model.entropies
                seed
    in
        case minimumEntropy of
            FoundAt minimumEntropyPos ->
                ( nextSeed, Continue, model )
            NotFound ->
                ( nextSeed, Finished, model )
            FoundContradiction ->
                ( nextSeed, Contradiction, model )



propagate
    :  Model
    -> Random.Seed
    -> ( Random.Seed, Model )
propagate model seed =
    ( seed, model )


run : Wave -> CheckBoundary -> Limit -> Model -> Random.Seed -> RunResolution
run wave checkBoundary (Limit limit) model seed =
    let

        iterate iterationsLeft ( prevSeed, prevObservation, prevModel ) =
            if iterationsLeft < 0 then ( prevSeed , prevObservation, prevModel )
            else case observe checkBoundary prevModel prevSeed of
                ( obsSeed, result, observedModel ) ->
                    if result == Continue then
                        case propagate observedModel obsSeed of
                            ( propSeed, propagatedModel ) ->
                                iterate (iterationsLeft - 1) ( propSeed, result, propagatedModel )
                    else ( obsSeed, result, observedModel )
    in
        case iterate limit ( seed, Continue, model ) of
                ( _, Continue, _ )  -> ReachedLimit (UnfinishedGraphics {})
                ( _, Finished, _ ) -> Success (Graphics {})
                ( _, Contradiction, _ ) -> GotContradiction



