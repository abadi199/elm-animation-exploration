module Easing exposing
    ( Easing
    , cubicBezier
    , default
    , ease
    , easeIn
    , easeInOut
    , easeOut
    , encode
    , linear
    , steps
    )

import Json.Encode as JE


type Easing
    = Linear
    | Ease
    | EaseIn
    | EaseOut
    | EaseInOut
    | CubicBezier
    | Steps Int


default : Easing
default =
    linear


linear : Easing
linear =
    Linear


ease : Easing
ease =
    Ease


easeIn : Easing
easeIn =
    EaseIn


easeOut : Easing
easeOut =
    EaseOut


easeInOut : Easing
easeInOut =
    EaseInOut


cubicBezier : Easing
cubicBezier =
    CubicBezier


steps : Int -> Easing
steps =
    Steps


encode : Easing -> JE.Value
encode easing =
    case easing of
        Linear ->
            JE.string "linear"

        Ease ->
            JE.string "ease"

        EaseIn ->
            JE.string "ease-in"

        EaseOut ->
            JE.string "ease-out"

        EaseInOut ->
            JE.string "ease-in-out"

        CubicBezier ->
            JE.string "TODO: cubicBezier"

        Steps n ->
            JE.string <| "steps(" ++ String.fromInt n ++ ")"
