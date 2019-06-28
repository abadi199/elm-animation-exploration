module Degree exposing
    ( Degree
    , deg
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
