module Kvant.Json.Options exposing (..)

import Kvant.Solver.Options as Options
import Kvant.Plane exposing (Plane(..), Boundary(..), Symmetry(..))


import Json.Decode as D
import Json.Encode as E


decodePatternSearch : D.Decoder Options.PatternSearch
decodePatternSearch =
    D.map3
        (\psize iboundary symmetry ->
            { patternSize = psize
            , boundary = iboundary
            , symmetry = symmetry
            }
        )
        (D.field "patternSize" <| D.map (\n -> (n, n)) <| D.int)
        (D.field "inputBoundary" D.string
            |> D.andThen
                (\boundary ->
                    case boundary of
                        "bounded" -> D.succeed Bounded
                        "periodic" -> D.succeed Periodic
                        _ -> D.fail <| "Invalid boundary: " ++ boundary
                )
        )
        (D.field "symmetry" D.string
            |> D.andThen
                (\symmetry ->
                    case symmetry of
                        "none" -> D.succeed NoSymmetry
                        "flip-only" -> D.succeed FlipOnly
                        "rotate-only" -> D.succeed RotateOnly
                        "flip-and-rotate" -> D.succeed FlipAndRotate
                        _ -> D.fail <| "Invalid symmetry: " ++ symmetry
                )
        )


decodeOutput : D.Decoder Options.Output
decodeOutput =
    D.map3
        (\oboundary width height ->
            ( oboundary, ( width, height ) )
        )
        (D.field "outputBoundary" D.string
            |> D.andThen (\boundary ->
                case boundary of
                    "bounded" -> D.succeed Bounded
                    "periodic" -> D.succeed Periodic
                    _ -> D.fail <| "Invalid boundary: " ++ boundary
            )
        )
        (D.field "outputWidth" D.int)
        (D.field "outputHeight" D.int)


encodePatternSearch : Options.PatternSearch -> E.Value
encodePatternSearch opts =
    E.object
        [

            ( "patternSize"
            , E.int <|
                case opts.patternSize of
                    ( n, _ ) -> n
            )
        ,

            ( "inputBoundary"
            , E.string <|
                case opts.boundary of
                    Bounded -> "bounded"
                    Periodic -> "periodic"
            )
        ,
            ( "symmetry"
            , E.string <|
                case opts.symmetry of
                    NoSymmetry -> "none"
                    FlipOnly -> "flip-only"
                    RotateOnly -> "rotate-only"
                    FlipAndRotate -> "flip-and-rotate"
            )

        ]


encodeOutput : Options.Output -> E.Value
encodeOutput ( boundary, size ) =
    E.object
        [
            ( "outputBoundary"
            , E.string <|
                case boundary of
                    Bounded -> "bounded"
                    Periodic -> "periodic"
            )
        ,
            ( "outputWidth"
            , E.int <|
                case size of
                    ( width, _ ) -> width
            )
        ,
            ( "outputHeight"
            , E.int <|
                case size of
                    ( _, height ) -> height
            )
        ]



