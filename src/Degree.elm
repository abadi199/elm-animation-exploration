module Degree exposing
    ( Degree
    , add
    , deg
    , randomGenerator
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


add : Degree -> Degree -> Degree
add (Degree a) (Degree b) =
    Degree (a + b)


randomGenerator : Degree -> Degree -> Random.Generator Degree
randomGenerator (Degree low) (Degree high) =
    Random.float low high
        |> Random.map deg
