module Px exposing
    ( Px
    , add
    , px
    , randomGenerator
    , toInt
    , toString
    )

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


randomGenerator : Int -> Int -> Random.Generator Px
randomGenerator low high =
    Random.int low high
        |> Random.map px
