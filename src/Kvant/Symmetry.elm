module Kvant.Symmetry exposing (..)


type Symmetry
    = I | X | L | T | Diagonal  -- Diag meaning slope like \


default : Symmetry
default = L


fromString : String -> Maybe Symmetry
fromString str =
    case str of
        "I" -> Just I
        "X" -> Just X
        "L" -> Just L
        "T" -> Just T
        "\\" -> Just Diagonal
        _ -> Nothing


symmetryToString : Symmetry -> String
symmetryToString symmetry =
    case symmetry of
        I -> "I"
        X -> "X"
        L -> "L"
        T -> "T"
        Diagonal -> "\\"
