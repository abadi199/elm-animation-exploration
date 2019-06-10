module Px exposing (Px, px, toInt, toString)


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
