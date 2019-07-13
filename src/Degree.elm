module Degree exposing
    ( Degree
    , deg
    , toFloat
    , toString
    )


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
