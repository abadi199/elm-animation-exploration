module Degree exposing
    ( Degree
    , add
    , deg
    , is
    , multiply
    , randomGenerator
    , subtract
    , toFloat
    , toString
    )

import Css
import Random


type Degree
    = Degree Float


deg : Float -> Degree
deg =
    Degree


toString : Degree -> String
toString (Degree degree) =
    String.fromFloat degree ++ "deg"


toFloat : Degree -> Float
toFloat (Degree n) =
    n


multiply : Float -> Degree -> Degree
multiply multiplier (Degree n) =
    Degree (n * multiplier)


add : Degree -> Degree -> Degree
add (Degree a) (Degree b) =
    Degree (a + b)


subtract : Degree -> Degree -> Degree
subtract (Degree a) (Degree b) =
    Degree (b - a)


randomGenerator : Degree -> Degree -> Random.Generator Degree
randomGenerator (Degree low) (Degree high) =
    Random.float low high
        |> Random.map deg


is : (Float -> Float -> Bool) -> Degree -> Degree -> Bool
is comparer (Degree right) (Degree left) =
    comparer left right
