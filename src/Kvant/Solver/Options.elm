module Kvant.Solver.Options exposing (..)


import Kvant.Vec2 exposing (Vec2)
import Kvant.Plane exposing (Plane(..), Size, Boundary(..), Symmetry(..))


import Json.Decode as D
import Json.Encode as E

-- import Xml.Decode as Xml


type alias Options =
    { approach : Approach
    , outputBoundary : Boundary
    , outputSize : Size
    }


type Approach
    = Overlapping
        { patternSize : Size -- FIXME: use just square patterns
        , inputBoundary : Boundary
        , symmetry : Symmetry -- FIXME: use in search
        -- TODO: ground : Int
        }
    | Tiled


decode : D.Decoder Options
decode =
    D.map4

        (\approach outputBoundary otputWidth outputHeight ->
            Options approach outputBoundary ( otputWidth, outputHeight )
        )

        (D.field "approach" D.string
            |> D.andThen
                (\approach ->
                    case approach of

                        "overlapping" ->
                            D.map3
                                (\psize iboundary symmetry ->
                                    Overlapping
                                        { patternSize = psize
                                        , inputBoundary = iboundary
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

                        "tiled" -> D.succeed Tiled
                        _ -> D.fail <| "Invalid approach: " ++ approach
                )
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


encode : Options -> E.Value
encode opts =
    E.object <|
        [
            ( "approach"
            , E.string <|
                case opts.approach of
                    Overlapping _ -> "overlapping"
                    Tiled -> "tiled"
            )
        ,
            ( "outputBoundary"
            , E.string <|
                case opts.outputBoundary of
                    Bounded -> "bounded"
                    Periodic -> "periodic"
            )
        ,
            ( "outputWidth"
            , E.int <|
                case opts.outputSize of
                    ( width, _ ) -> width
            )
        ,
            ( "outputHeight"
            , E.int <|
                case opts.outputSize of
                    ( _, height ) -> height
            )
        ] ++

        case opts.approach of
            Overlapping oopts ->
                [

                    ( "patternSize"
                    , E.int <|
                        case oopts.patternSize of
                            ( n, _ ) -> n
                    )
                ,

                    ( "inputBoundary"
                    , E.string <|
                        case oopts.inputBoundary of
                            Bounded -> "bounded"
                            Periodic -> "periodic"
                    )
                ,
                    ( "symmetry"
                    , E.string <|
                        case oopts.symmetry of
                            NoSymmetry -> "none"
                            FlipOnly -> "flip-only"
                            RotateOnly -> "rotate-only"
                            FlipAndRotate -> "flip-and-rotate"
                    )

                ]
            Tiled ->
                []
