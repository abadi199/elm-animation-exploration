module Millisecond exposing
    ( Millisecond
    , add
    , encode
    , fromSecond
    , isAfter
    , millisecond
    , toInt
    , toString
    )

import Json.Encode as JE
import Second exposing (Second)


type Millisecond
    = Millisecond Int


millisecond =
    Millisecond


isAfter : Millisecond -> Millisecond -> Bool
isAfter (Millisecond from) (Millisecond to) =
    from > to


add : Millisecond -> Millisecond -> Millisecond
add (Millisecond a) (Millisecond b) =
    Millisecond (a + b)


toString : Millisecond -> String
toString (Millisecond ms) =
    String.fromInt ms ++ "ms"


fromSecond : Second -> Millisecond
fromSecond second =
    second
        |> Second.toFloat
        |> (*) 1000
        |> round
        |> millisecond


toInt : Millisecond -> Int
toInt (Millisecond ms) =
    ms


encode : Millisecond -> JE.Value
encode (Millisecond ms) =
    JE.int ms
