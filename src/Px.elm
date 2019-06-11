module Px exposing
    ( Px
    , add
    , px
    , toInt
    , toString
    )


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
