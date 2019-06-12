module Second exposing
    ( Second
    , add
    , second
    , toFloat
    , toString
    )


type Second
    = Second Float


second : Float -> Second
second =
    Second


toString : Second -> String
toString (Second s) =
    String.fromFloat s ++ "s"


toFloat : Second -> Float
toFloat (Second s) =
    s


add : Second -> Second -> Second
add (Second a) (Second b) =
    Second (a + b)
