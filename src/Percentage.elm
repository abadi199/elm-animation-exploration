module Percentage exposing
    ( Percentage
    , fromFloat
    , fromSecond
    , toFloat
    , toString
    )

import Second exposing (Second)


type Percentage
    = Percentage { numerator : Float, denominator : Float }


fromFloat : Float -> Percentage
fromFloat float =
    Percentage { numerator = float, denominator = 1 }


fromSecond : { numerator : Second, denominator : Second } -> Percentage
fromSecond { numerator, denominator } =
    Percentage
        { numerator = Second.toFloat numerator
        , denominator = Second.toFloat denominator
        }


toString : Percentage -> String
toString (Percentage { numerator, denominator }) =
    ((numerator / denominator) * 100)
        |> round
        |> String.fromInt
        |> (\pct -> pct ++ "%")


toFloat : Percentage -> Float
toFloat (Percentage { numerator, denominator }) =
    (numerator / denominator) * 100
