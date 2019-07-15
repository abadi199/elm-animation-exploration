module Px exposing
    ( Px
    , add
    , divideBy
    , is
    , map
    , multiply
    , negate
    , px
    , randomGenerator
    , toElmCss
    , toFloat
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


toFloat : Px -> Float
toFloat =
    toInt >> Basics.toFloat


add : Px -> Px -> Px
add (Px a) (Px b) =
    Px (a + b)


divideBy : Int -> Px -> Px
divideBy denominator (Px numerator) =
    numerator // denominator |> Px


randomGenerator : Px -> Px -> Random.Generator Px
randomGenerator (Px low) (Px high) =
    Random.int low high
        |> Random.map px


toElmCss : Px -> Css.Px
toElmCss (Px n) =
    Css.px (Basics.toFloat n)


negate : Px -> Px
negate (Px n) =
    Px -n


multiply : Float -> Px -> Px
multiply multiplier (Px number) =
    Basics.toFloat number * multiplier |> round |> Px


map : (Int -> Int) -> Px -> Px
map f (Px n) =
    Px (f n)


is : (Int -> Int -> Bool) -> Px -> Px -> Bool
is comparer (Px right) (Px left) =
    comparer left right
