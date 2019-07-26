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


type Observation
    = Continue -- null
    | Contradiction -- false
    | Finished -- true


type Graphics = Graphics {}
type UnfinishedGraphics = UnfinishedGraphics {}
type RunResolution
    = FoundContradiction
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
    { ones : List Int
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
    , entropies : List Float
    , startingEntropy : Float
    , stack : List Int
    , stackSize : Int
    -- , startingEntropy : Float
    }


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
            { ones = List.repeat count 0
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
        , entropies = List.repeat count 0
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


observe
    :  CheckBoundary
    -> Model
    -> Random.Seed
    -> ( Random.Seed, Observation, Model )
observe checkBoundary model seed =
    let
        findMinimumEntropy ( index, (amount, entropy) ) maybeVals =
            if checkBoundary { x = 0, y = 0 } then maybeVals
            else case ( amount, maybeVals ) of
                ( 0, _ ) -> Nothing
                ( _, Nothing ) -> Nothing
                ( nonZeroAmount, Just lastValues ) ->
                    if nonZeroAmount > 1 && entropy <= lastValues.min then
                        let
                            ( noise, nextSeed ) =
                                Random.step
                                    (Random.float 0 1 |> Random.map ((*) 0.000001))
                                    lastValues.seed
                        in
                            if entropy + noise < lastValues.min
                                then
                                    { lastValues
                                    | min = entropy + noise
                                    , indexOfMin = index
                                    , seed = nextSeed
                                    } |> Just
                                else
                                    { lastValues
                                    | seed = nextSeed
                                    } |> Just
                    else maybeVals
        maybeIndexOfMin =
            List.map2
                Tuple.pair
                model.sums.ones
                model.entropies
                |> List.indexedMap Tuple.pair
                |> List.foldl
                    findMinimumEntropy
                    (Just
                        { seed = seed
                        , min = Random.minInt |> toFloat
                        , indexOfMin = -1
                        })
                |> Maybe.map .indexOfMin
    in
        ( seed, Finished, model )


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
                ( _, Contradiction, _ ) -> FoundContradiction



