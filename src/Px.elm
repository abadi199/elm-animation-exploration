module Px exposing
    ( Px
    , add
    , divideBy
    , map
    , multiply
    , negate
    , px
    , randomGenerator
    , toElmCss
    , toInt
    , toString
    )

import Css
import Random


type Px
    = Px Int


px : Int -> Px
px =
    Px


toString : Px -> String
toString (Px n) =
    String.fromInt n ++ "px"


toInt : Px -> Int
toInt (Px n) =
    n


add : Px -> Px -> Px
add (Px a) (Px b) =
    Px (a + b)


divideBy : Int -> Px -> Px
divideBy denominator (Px numerator) =
    numerator // denominator |> Px


randomGenerator : Int -> Int -> Random.Generator Px
randomGenerator low high =
    Random.int low high
        |> Random.map px


toElmCss : Px -> Css.Px
toElmCss (Px n) =
    Css.px (toFloat n)


negate : Px -> Px
negate (Px n) =
    Px -n


multiply : Float -> Px -> Px
multiply multiplier (Px number) =
    toFloat number * multiplier |> round |> Px


map : (Int -> Int) -> Px -> Px
map f (Px n) =
    Px (f n)
