module Easing exposing
    ( Easing
    , cubicBezier
    , ease
    , easeIn
    , easeInOut
    , easeOut
    , linear
    )


type Easing
    = Linear
    | Ease
    | EaseIn
    | EaseOut
    | EaseInOut
    | CubicBezier


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
