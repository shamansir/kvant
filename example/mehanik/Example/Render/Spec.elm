module Example.Render.Spec exposing (..)


import Html exposing (Html)
import Canvas exposing (Renderable)
import Canvas as Canvas exposing (toHtml)


type alias Spec v a target =
    { default : a
    , contradiction : a
    , v : v -> target
    , a : a -> target
    , merge : List a -> a
    , scaled : Float -> a -> target
    , vToString : v -> String
    }


type alias HtmlSpec v a msg = Spec v a (Html msg)

type alias CanvasSpec v a = Spec v a Renderable


map : (targetA -> targetB) -> Spec v a targetA -> Spec v a targetB
map f s =
    { default = s.default
    , contradiction = s.contradiction
    , v = f << s.v
    , a = f << s.a
    , merge = s.merge
    , scaled = \n a -> f <| s.scaled n a
    , vToString = s.vToString
    }


-- toHtml : Spec v a Renderable -> Spec v a (Html msg)
-- toHtml = map <| Canvas.toHtml (100, 100) [] << List.singleton
